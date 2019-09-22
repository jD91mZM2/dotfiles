{ pkgs ? import <nixpkgs> {} }:

let
  stow = pkgs.writeShellScriptBin "stow" ''
    "${pkgs.stow}/bin/stow" -t ~ "$@"
  '';
  deploy = pkgs.writeShellScriptBin "deploy" ''
    "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
  '';
in
pkgs.mkShell {
  # Things to be put in $PATH
  nativeBuildInputs = [ stow deploy ];
}
