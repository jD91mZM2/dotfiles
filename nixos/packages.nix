{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Look & Feel
    adapta-backgrounds
    adapta-gtk-theme
    libsForQt5.qtstyleplugins # uniform QT/GTK look
    numix-icon-theme-circle
    xorg.xcursorthemes

    # Graphical - System
    compton
    dmenu
    j4-dmenu-desktop
    networkmanagerapplet

    # Graphical - Applications
    firefox
    gnome3.seahorse
    kdeApplications.konsole
    pavucontrol
    xfce.thunar

    # Graphical - Utils
    feh
    maim

    # Applications
    grml-zsh-config
    mosh
    ncdu
    neovim

    # Utils
    bind
    binutils
    fd
    file
    gist
    git
    gitAndTools.hub
    gnupg
    htop
    z
    patchelf
    ripgrep
    socat
    trash-cli
    tree
    unzip
    wget
    xclip
    zip
  ];
  fonts.fonts = with pkgs; [
    cantarell-fonts
    font-awesome-ttf
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];
}
