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
    #
    # TODO: This is copied (and fixed) from flake.nix. Update when home-manager
    # gets flake support
    nixpkgs.overlays =
      [
        (import (builtins.fetchTarball "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz"))
      ]
      ++ (
        map
          (name: import (<dotfiles/overlays> + "/${name}"))
          (builtins.attrNames (builtins.readDir <dotfiles/overlays>))
      );

    xdg.configFile = {
      # Install overlays
      "nixpkgs/overlays".source = config.lib.file.mkOutOfStoreSymlink (<dotfiles/overlays>);

      # Install this home-manager config
      "nixpkgs/home.nix".source = config.lib.file.mkOutOfStoreSymlink cfg.source;
    };

    # Don't mess with my keyboard layout!
    home.keyboard = null;
  };
}