{ pkgs, ... }:
{
  # More involved programs
  programs = {
    adb.enable = true;
    dconf.enable = true;
    slock.enable = true;
    powerline-rs.package = pkgs.powerline-rs;
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
    firefox
    gnome3.zenity
    virtmanager
    xfce.xfce4-power-manager

    # Command line
    tmux
    neovim # nice to have as backup if emacs would break

    # Must have utils
    bind
    binutils
    cryptsetup
    efibootmgr
    gist
    httpie
    manpages
    mosh
    nix-index
    patchelf
    pciutils
    socat
    unzip
    wget
    xclip
    xdotool
    zip

    # Daemons
    udiskie
  ];
}
