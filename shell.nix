{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = with pkgs.buildPackages; [
        dune_3
        ocaml
        ocamlPackages.ppx_deriving
        ocamlPackages.utop
        ocamlPackages.findlib
        ocamlPackages.ocamlformat
        ocamlPackages.core
        ocamlPackages.angstrom
    ];
}
