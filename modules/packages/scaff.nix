{ pkgs, lib, inputs, system, ... }:
with lib;
let
  scaff-unwrapped = inputs.scaff.defaultPackage."${system}";

  repo = "https://gitlab.com/jD91mZM2/scaff-repo/-/jobs/932530814/artifacts/raw/build/config.toml";

  scaff = pkgs.symlinkJoin {
    name = "scaff";
    paths = [ scaff-unwrapped ];

    nativeBuildInputs = [ pkgs.makeWrapper ];

    postBuild = ''
      rm "$out/bin/scaff"
      makeWrapper "${scaff-unwrapped}/bin/scaff" "$out/bin/scaff" --add-flags "-i ${escapeShellArg repo}"
    '';
  };
in
{
  environment.systemPackages = [
    scaff
  ];
}
