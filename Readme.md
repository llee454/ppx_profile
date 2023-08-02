Profile PPX Readme
==================

The Profile PPX makes it easier for OCaml developers to profile their code. This allows them to see how much time their programs our spending executing various expressions and to determine the number of times these expressions are executed. This package provides both a convienent syntax for embedding profile inspection points and generates profiling reports that directly highlight the source of pain in programs.

Usage
-----

To use the Profile PPX in your OCaml projects:

1. add the `ppx_profile_runtime` library to your Dune file's `libraries` list
2. add the `ppx_profile` preprocessor to your Dune file's `preprocess` list

The following Dune file illustrates how to add Profile PPX to your Dune file:

```
(executable
  (name main)
  (libraries
    ppx_profile_runtime
  )
  (preprocess (pps
    ppx_profile
  ))
)
```

3. wrap any expression that you want to profile in `[%profile ...]`.

For example:

``` ocaml
  let example_function x = [%profile x + 2]
```

4. wrap any LWT promise that you want to profile in `[%profile_lwt ...]`
5. call `Ppx_profile_runtime.annotate ()` at the end of your program

For example:

``` ocaml
let () = Lwt_main.run @@ [%profile_lwt execute_program] >>= Ppx_profile_runtime.annotate
```

6. when the program is executed (using for example `dune exec program.exe`), the `annotate` function will copy all of the source code files that contain profile inspection expressions into new files that include a ".profile.ml" suffix. For every profile expression, the function will print out the expression's profiling statistics.

Initializing the Build Environment
----------------------------------

```
opam switch create . 5.0.0+options --no-install
opam update
opam install --deps-only . -y
dune build
```