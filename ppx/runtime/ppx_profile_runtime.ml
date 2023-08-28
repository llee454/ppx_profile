(*
  This module defines datastrutures and functions for profiling code.
  Developers can use these definitions to print out the number of
  times certain expressions are executed and the amount of time spent
  executing them.
*)

open! Core
open! Lwt.Syntax
open! Lwt.Infix

type stats = {
  num_executions: int;
  total_time: int;
}
[@@deriving sexp, compare, hash]

type position = Lexing.position

let compare_position ({ pos_lnum = x; _ } : Lexing.position) ({ pos_lnum = y; _ } : Lexing.position) =
  [%compare: int] x y

let position_of_sexp sexp =
  let pos_fname, pos_lnum, pos_bol, pos_cnum = [%of_sexp: string * int * int * int] sexp in
  Lexing.{ pos_fname; pos_lnum; pos_bol; pos_cnum }

let sexp_of_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
  [%sexp_of: string * int * int * int] (pos_fname, pos_lnum, pos_bol, pos_cnum)

let position_to_string (position : Lexing.position) = sprintf !"%{Source_code_position}" position

let hash_fold_position state position = position_to_string position |> hash_fold_string state

let hash_position (position : Lexing.position) = position_to_string position |> hash_string

module Table = Hashtbl.Make (struct
  type t = position [@@deriving compare, sexp, hash]
end)

(**
  Represents the profiler.

  The profiler is a table that lists the stats for each of the control
  points. Each entry represents a control point. The key stores the
  control points source code position. THe data stores the stats for
  that control point.
*)
let profiler = Table.create ()

(**
  Executes the given function and returns the result along with the
  number of microseconds that elapsed.
*)
let time ~(f : unit -> 'a) : 'a * int =
  let start_ts = Time_now.nanoseconds_since_unix_epoch () in
  let res = f () in
  let end_ts = Time_now.nanoseconds_since_unix_epoch () in
  let time = Int63.(end_ts - start_ts) |> Int63.to_int_exn in
  res, time / 1_000

(**
  Accepts two arguments: here, a source coude position; and f, a
  function; executes f and updates the profiler statistics for f's
  source code position.

  Note: we recommend using the [%here] PPX to get the here value.
*)
let inspect ~(here : Lexing.position) ~(f : unit -> 'a) : 'a =
  let res, time = time ~f in
  Hashtbl.update profiler here ~f:(function
    | Some { num_executions; total_time } ->
      { num_executions = num_executions + 1; total_time = total_time + time }
    | None -> { num_executions = 1; total_time = time });
  res

let time_lwt ~(f : unit -> 'a Lwt.t) : ('a * int) Lwt.t =
  let start_ts = Time_now.nanoseconds_since_unix_epoch () in
  let+ res = f () in
  let end_ts = Time_now.nanoseconds_since_unix_epoch () in
  let time = Int63.(end_ts - start_ts) |> Int63.to_int_exn in
  res, time / 1_000

(**
  Accepts two arguments: here, a source coude position; and f, an
  Lwt promise; awaits f and updates the profiler statistics for f's
  source code position.

  Note: we recommend using the [%here] PPX to get the here value.
*)
let inspect_lwt ~(here : Lexing.position) ~(f : unit -> 'a Lwt.t) : 'a Lwt.t =
  let+ res, time = time_lwt ~f in
  Hashtbl.update profiler here ~f:(function
    | Some { num_executions; total_time } ->
      { num_executions = num_executions + 1; total_time = total_time + time }
    | None -> { num_executions = 1; total_time = time });
  res

let int_to_string = Int.to_string_hum ~delimiter:','

let sexp_of_stats { num_executions; total_time } =
  Sexp.List
    [
      Sexp.List [ Sexp.Atom "n"; Sexp.Atom (int_to_string num_executions) ];
      Sexp.List [ Sexp.Atom "total_time"; Sexp.Atom (int_to_string (total_time / 1000)); Sexp.Atom "ms" ];
    ]

(**
  Creates profiler reports for every source code file that contains an
  inspection point. For each file that contains a profiler inspect
  function call, create a new file that has the same name but appends
  the ".profile" suffix, and inserts a comment at the line containing
  the inspect function call that reports the number of times the code
  was executed and how long the program spent executing it.
*)
let annotate () =
  Hashtbl.fold profiler ~init:String.Map.empty ~f:(fun ~key:{ pos_fname; pos_lnum; _ } ~data:stats acc ->
      Map.update acc pos_fname ~f:(function
        | Some annotations -> Map.add_multi annotations ~key:pos_lnum ~data:stats
        | None -> Int.Map.singleton pos_lnum [ stats ]))
  |> Map.to_alist
  |> Lwt_list.iter_s (fun (filename, annotations) ->
         let lnum = ref 0 in
         let output_filename =
           (if Filename.check_suffix filename ".ml" then Filename.chop_suffix filename ".ml" else filename)
           |> sprintf "%s.profile.ml"
         in
         Lwt_io.lines_of_file filename
         |> Lwt_stream.map_list (fun curr_line ->
                incr lnum;
                Map.find annotations !lnum |> function
                | Some stats ->
                  [ [%sexp_of: stats list] stats |> sprintf !"(* PROFILER %{Sexp} *)"; curr_line ]
                | None -> [ curr_line ])
         |> Lwt_io.lines_to_file output_filename)