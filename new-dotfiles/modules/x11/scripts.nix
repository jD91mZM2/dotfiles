{ pkgs, lib, config, ... }:

with lib;
let
  # List of arguments to substituteInPlace
  args = imap0
    (i: colour: ''
      --subst-var-by "base${toString i}" "${colour}" \
    '')
    config.globals.colourscheme.colours;

  # Scripts package
  package = pkgs.runCommand "scripts" { } ''
    cp -r ${./scripts} "$out"
    for f in $out/*; do
      substituteInPlace "$f" \
        ${toString args}
        # empty line to allow trailing slash
    done
  '';
in
{
  home = {
    # Install scripts to ~/.local/share
    xdg.dataFile."scripts".source = package;
  };
}
