{ pkgs, lib, inputs, system, ... }:
with lib;
let
  inc-unwrapped = inputs.inc.defaultPackage."${system}";

  repo = "/home/user/Coding/Rust/inc-repo/templates";

  inc = pkgs.symlinkJoin {
    name = "inc";
    paths = [ inc-unwrapped ];

    nativeBuildInputs = [ pkgs.makeWrapper ];

    postBuild = ''
      rm "$out/bin/inc"
      makeWrapper "${inc-unwrapped}/bin/inc" "$out/bin/inc" \
        --set INC_REPO ${lib.escapeShellArg repo}
    '';
  };
in
{
  environment.systemPackages = [
    inc
  ];
}
