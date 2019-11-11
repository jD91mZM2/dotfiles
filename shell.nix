{ pkgs ? import <nixpkgs> {} }:

let
  deploy = pkgs.writeShellScriptBin "deploy" ''
    "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
  '';
  stow = pkgs.writeShellScriptBin "stow" ''
    "${pkgs.stow}/bin/stow" -t ~ "$@"
  '';
in
pkgs.mkShell {
  # Things to be put in $PATH
  nativeBuildInputs =
    # Convenient scripts
    [
      deploy
      stow
    ]

    ++

    (with pkgs; [
    ]);
}
