{ pkgs, lib, config, ... }:

with lib;
let
  # List of arguments to substituteInPlace
  colourschemeArgs = imap0
    (i: colour: '' --subst-var-by "base${toString i}" "${colour}" '')
    config.globals.colourscheme.colours;

  pkgArg = pkg: name: '' --subst-var-by "${name}" "${pkg}/bin/${name}" '';

  # Scripts package
  package = pkgs.runCommand "scripts"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    cp -r ${./scripts} "$out"
    for f in "$out"/*; do
      substituteInPlace "$f" \
        ${concatStringsSep "\\\n" colourschemeArgs} \
        ${pkgArg pkgs.dmenu "dmenu"} \
        ${pkgArg pkgs.xclip "xclip"} \
        ${pkgArg pkgs.xdotool "xdotool"} \
        ${pkgArg pkgs.xorg.xrandr "xrandr"} \
        ${pkgArg pkgs.imagemagick "import"} \
        ${pkgArg pkgs.mpv "mpv"} \
        ${pkgArg pkgs.j4-dmenu-desktop "j4-dmenu-desktop"}
    done
  '';
in
{
  home = {
    # Install scripts to ~/.local/share
    xdg.dataFile."scripts".source = package;
  };
}
