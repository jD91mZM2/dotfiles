{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkShell {
  buildInputs = [];
}
