{ config, lib, pkgs, ... }:

let
  setup-doom = pkgs.writeShellScript "setup-doom" ''
    dir="''${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
    if [ ! -e "$dir" ]; then
      ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git "$dir"
      "$dir/bin/doom" install --no-env --no-config --no-fonts
    fi
  '';
  emacs = pkgs.runCommand "emacs" {
    nativeBuildInputs = with pkgs; [ makeWrapper ];
  } ''
    cp -r "${pkgs.emacsUnstable}" "$out"
    chmod +w "$out/bin"
    wrapProgram "$out/bin/emacs" --run "${setup-doom}"
  '';
in
{
  xdg.configFile = {
    "doom".source = config.lib.file.mkOutOfStoreSymlink ./doom-config;
  };

  home.packages = [ emacs ];
}
