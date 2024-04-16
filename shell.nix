{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = with pkgs.buildPackages; [
        ocaml
        ocamlPackages.utop
        ocamlPackages.findlib
        ocamlPackages.ocamlformat
        ocamlPackages.angstrom
    ];
}
