{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = with pkgs.buildPackages; [
        clang
        libllvm
        dune_3
        ocaml
        ocamlPackages.utop
        ocamlPackages.ppx_deriving
        ocamlPackages.ppx_inline_test
        ocamlPackages.ppx_jane
        ocamlPackages.findlib
        ocamlPackages.ocamlformat
        ocamlPackages.core
        ocamlPackages.core_unix
        ocamlPackages.angstrom
    ];
}
