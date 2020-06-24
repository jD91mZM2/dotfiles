{ config, lib, pkgs, ... }:

{
  xdg.configFile = {
    "doom".source = config.lib.file.mkOutOfStoreSymlink ./doom-config;
  };

  home.packages = with pkgs; [
    emacsUnstable
    emacs-all-the-icons-fonts
  ];

  home.activation = {
    installDoomEmacs = lib.hm.dag.entryAnywhere ''
      dir="''${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
      if [ ! -e "$dir" ]; then
        ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git "$dir"
        "$dir/bin/doom" install --no-envvar --no-config --no-fonts
      fi
    '';
  };
}
