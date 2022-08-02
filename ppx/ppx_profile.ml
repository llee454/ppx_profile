open Ppxlib

let profile =
  Extension.V3.declare "profile" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt payload ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let here_loc = Ppx_here_expander.lift_position ~loc in
      [%expr Ppx_profile_runtime.inspect ~here:[%e here_loc] ~f:(fun () -> [%e payload])])

let profile_lwt =
  Extension.V3.declare "profile_lwt" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt payload ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let here_loc = Ppx_here_expander.lift_position ~loc in
      [%expr Ppx_profile_runtime.inspect_lwt ~here:[%e here_loc] ~f:(fun () -> [%e payload])])

let () =
  Driver.register_transformation ~rules:[ Ppxlib.Context_free.Rule.extension profile ] "profile";
  Driver.register_transformation ~rules:[ Ppxlib.Context_free.Rule.extension profile_lwt ] "profile_lwt"
