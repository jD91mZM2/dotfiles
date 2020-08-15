{ pkgs, config, lib, ... }:

let
  nur-no-pkgs = import (<dotfiles/shared/nur-no-pkgs.nix>);
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
    nur-no-pkgs.repos.jd91mzm2.hm-modules.programs

    ./fonts.nix
    ./emacs.nix
    ./env.nix
    ./firefox.nix
    ./graphics.nix
    ./misc.nix
    ./packages.nix
    ./shells.nix
  ];

  config = {
    # Set up master branch of home-manager
    programs.home-manager = {
      enable = true;
      path   = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };

    # Load overlays
    nixpkgs.overlays = (import <dotfiles>).overlays;

    # g_get_user_special_dir(...) would return NULL, see
    # https://github.com/NixOS/nixpkgs/issues/95276
    xdg.userDirs.enable = true;

    xdg.configFile = {
      # Install this home-manager config
      "nixpkgs/home.nix".source = config.lib.file.mkOutOfStoreSymlink cfg.source;
    };

    # Don't mess with my keyboard layout!
    home.keyboard = null;
  };
}
