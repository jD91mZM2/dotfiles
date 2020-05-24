{ pkgs, lib, ... }:

let
  nur-no-pkgs = import (<dotfiles/shared/nur-no-pkgs.nix>);
in
{
  imports = [
    nur-no-pkgs.repos.jd91mzm2.hm-modules.programs
    nur-no-pkgs.repos.jd91mzm2.hm-modules.directlink

    ./dunst.nix
    ./emacs.nix
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

  # System overlays
  nixpkgs.overlays = import ./overlays.nix;
  xdg.configLink."nixpkgs/overlays".source = ./overlays;

  # Don't mess with my keyboard layout!
  home.keyboard = null;
}
