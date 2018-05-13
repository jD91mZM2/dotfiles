{ pkgs, ... }:
let
  unstable = import <nixos-unstable> {};
in {
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
    xfce.xfce4-pulseaudio-plugin
    xfce.xfce4panel_gtk3
    xfce.xfconf
    ## Déjà Dup
    deja-dup
    gnome3.dconf

    # Graphical - Applications
    firefox
    gnome3.seahorse
    mpv
    pavucontrol
    xfce.thunar
    xfce.xfce4-power-manager
    (unstable.st.override {
      conf = builtins.readFile (substituteAll {
        src = ./st/config.h;
        colorscheme = builtins.readFile (fetchFromGitHub {
          owner = "honza";
          repo = "base16-st";
          rev = "b3d0d4fbdf86d9b3eda06f42a5bdf261b1f7d1d1";

          sha256 = "1z08abn9g01nnr1v4m4p8gp1j8cwlvcadgpjb7ngjy8ghrk8g0sh";
        } + "/build/base16-default-dark-theme.h");
        shell = ./st/tmux.sh;
      });
    })

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
    ffmpeg
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
    xdotool
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
