{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Graphical - Look & Feel
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
    keepassxc
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
    gnome3.zenity

    # Applications
    grml-zsh-config
    mosh
    ncdu
    neovim
    tmux
    weechat
    ## NeoVim
    neovim
    python27Packages.neovim
    python36Packages.neovim

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
    httpie
    neofetch
    nix-index
    patchelf
    ripgrep
    socat
    trash-cli
    tree
    unzip
    wget
    xclip
    youtube-dl
    zip

    # Languages
    cabal-install
    ghc
    python
    ruby
    rustup

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
