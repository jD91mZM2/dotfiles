{ pkgs, ... }:
let
  nur-no-pkgs = (import <dotfiles/shared/nur-no-pkgs.nix>);
in
{
  imports = [
    nur-no-pkgs.repos.jd91mzm2.modules.programs
  ];

  # System overlays
  nixpkgs.overlays = let
    dir = (<dotfiles/home/nixpkgs/.config/nixpkgs/overlays>);
    names = builtins.attrNames (builtins.readDir dir);
  in
    (map (name: import (dir + "/${name}")) names);

  # More involved programs
  programs = {
    adb.enable = true;
    dconf.enable = true;
    slock.enable = true;
    powerline-rs.package = pkgs.master.powerline-rs;
  };
  virtualisation = {
    docker = {
      enable           = true;
      autoPrune.enable = true;
      storageDriver    = "zfs";
    };
    libvirtd.enable = true;
  };

  # Packages
  environment.systemPackages = with pkgs; [
    # Graphical - WM
    compton
    dmenu
    dunst
    feh
    j4-dmenu-desktop
    networkmanagerapplet

    # Graphical
    emacs
    firefox
    gnome3.zenity
    nur.repos.jd91mzm2.st
    virtmanager
    xfce.xfce4-power-manager

    # Command line
    direnv
    tmux

    # Must have utils
    autojump
    bc
    bind
    binutils
    cryptsetup
    efibootmgr
    gist
    httpie
    manpages
    mosh
    ncdu
    nix-index
    patchelf
    pciutils
    socat
    trash-cli
    unzip
    wget
    xclip
    xdotool
    zip

    # Daemons
    udiskie
  ];
}
