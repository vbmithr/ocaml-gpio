#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "gpio" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/gpio";
    Pkg.bin ~auto:true "lib_test/test";
    Pkg.bin ~auto:true "lib_test/probe";
  ]
