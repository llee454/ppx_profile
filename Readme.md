Profile PPX Readme
==================

The Profile PPX makes it easier for OCaml developers to profile their code. This allows them to see how much time their programs our spending executing various expressions and to determine the number of times these expressions are executed. This package provides both a convienent syntax for embedding profile inspection points and generates profiling reports that directly highlight the source of pain in programs.

Usage
-----

To use the Profile PPX in your OCaml projects add the `ppx_profile` preprocessor to your Dune file's `preprocess` list

The following Dune file illustrates how to add Profile PPX to your Dune file:

```
(executable
  (name main)
  (libraries
    ...
  )
  (preprocess (pps
    ppx_profile
  ))
)
```

You must also update your OPAM package configuration file. Add the following line to your "pin-depends" section: ["ppx_profile.1.0.0" "git+https://github.com/llee454/ppx_profile.git#main"]. Add the following to your "depends" section: "ppx_profile" { = "1.0.0"}.

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
opam switch create . ocaml-variants.4.10.0+flambda --no-install
opam update
opam install --deps-only .
dune build
```
