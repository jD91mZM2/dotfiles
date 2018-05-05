{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Look & Feel
    adapta-backgrounds
    adapta-gtk-theme
    libsForQt5.qtstyleplugins # uniform QT/GTK look
    numix-icon-theme
    numix-icon-theme-circle
    xorg.xcursorthemes

    # Graphical - System
    compton
    dmenu
    dunst
    j4-dmenu-desktop
    networkmanagerapplet
    ## XFCE Panel
    xfce.xfce4-battery-plugin
    xfce.xfce4-panel
    xfce.xfce4-pulseaudio-plugin
    xfce.xfconf
    ## Déjà Dup
    deja-dup
    gnome3.dconf

    # Graphical - Applications
    firefox
    gnome3.seahorse
    kdeApplications.konsole
    mpv
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
    autojump
    bind
    binutils
    fd
    file
    gist
    git
    gitAndTools.hub
    gnupg
    htop
    patchelf
    ripgrep
    socat
    trash-cli
    tree
    unzip
    wget
    xclip
    zip

    # Daemons
    udiskie
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
