{ pkgs, config, ... }:

let
  nur-no-pkgs = import (<dotfiles/shared/nur-no-pkgs.nix>);
in
{
  imports = [
    nur-no-pkgs.repos.jd91mzm2.hm-modules.programs

    ./emacs
    ./env.nix
    ./firefox.nix
    ./graphics.nix
    ./misc.nix
    ./packages.nix
    ./polybar.nix
    ./shells.nix
  ];

  # Set up master branch of home-manager
  programs.home-manager = {
    enable = true;
    path   = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };

  # Load overlays
  nixpkgs.overlays = import ./overlays.nix;

  xdg.configFile = {
    # Install overlays
    "nixpkgs/overlays".source = config.lib.file.mkOutOfStoreSymlink ./overlays;

    # Install this home-manager config
    "nixpkgs/home.nix".source = config.lib.file.mkOutOfStoreSymlink ./default.nix;
  };

  # Don't mess with my keyboard layout!
  home.keyboard = null;
}
