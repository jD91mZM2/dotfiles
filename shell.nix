{
  pkgs ? import <nixpkgs> {},
  home-manager ? (pkgs.callPackage (builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz) {}).home-manager,
}:

pkgs.mkShell {
  # Things to be put in $PATH
  nativeBuildInputs =
    # Convenient scripts
    [
      (pkgs.writeShellScriptBin "deploy" ''
        "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
      '')
      (pkgs.writeShellScriptBin "home-manager" ''
        "${home-manager}/bin/home-manager" -f ${toString ./home} "$@"
      '')
    ];
}
