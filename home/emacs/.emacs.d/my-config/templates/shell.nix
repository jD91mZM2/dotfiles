{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Things to be put in $PATH
  nativeBuildInputs = [];

  # Libraries to be installed
  buildInputs = [];
}
