{ config, lib, pkgs, ... }:

let
  emacs-bin = pkgs.writeShellScriptBin "emacs" ''
    dir="''${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
    if [ ! -e "$dir" ]; then
      ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git "$dir"
      "$dir/bin/doom" install --no-env --no-config --no-fonts
    fi

    ${pkgs.emacsUnstable}/bin/emacs "$@"
  '';
  emacs = pkgs.symlinkJoin {
    name = "emacs";
    paths = [
      emacs-bin
      (pkgs.makeDesktopItem {
        name = "emacs";
        desktopName = "Emacs";
        exec = "${emacs-bin}/bin/emacs";
      })
    ];
  };
in
{
  xdg.configFile = {
    "doom".source = config.lib.file.mkOutOfStoreSymlink ./doom-config;
  };

  home.packages = [ emacs ];
}
