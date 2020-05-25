{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Things to be put in $PATH
  nativeBuildInputs =
    # Convenient scripts
    [
      (pkgs.writeShellScriptBin "deploy" ''
        "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
      '')
    ];
}
