{ pkgs, config, lib, ... }:

let
  cfg = config.setup;
in {
  options.setup = with lib; {
    source = mkOption {
      type = types.path;
      description = ''
        Where to bind ~/.config/nixpkgs/home.nix
      '';
    };
  };

  imports = [
    ./emacs.nix
    ./env.nix
    ./firefox.nix
    ./fonts.nix
    ./graphics.nix
    ./misc.nix
    ./shells.nix
  ];

  config = {
    # Set up master branch of home-manager
    # programs.home-manager = {
    #   enable = true;
    #   path   = https://github.com/rycee/home-manager/archive/master.tar.gz;
    # };

    # g_get_user_special_dir(...) would return NULL, see
    # https://github.com/NixOS/nixpkgs/issues/95276
    xdg.userDirs.enable = true;

    # Don't mess with my keyboard layout!
    home.keyboard = null;
  };
}
