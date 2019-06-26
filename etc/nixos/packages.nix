{ pkgs, ... }:
let
  unstable = import <nixos-unstable> {};
in
{
  # Services
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.upower.enable = true;
  services.locate = {
    enable = true;
    interval = "07:00 PM";
  };

  # More involved programs
  programs = {
    adb.enable = true;
    dconf.enable = true;
    slock.enable = true;
    bash = {
      enableCompletion = true;
      interactiveShellInit = ''
        source "${pkgs.autojump}/share/autojump/autojump.bash"
        eval "$("${pkgs.direnv}/bin/direnv" hook bash)"
      '';
    };
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      promptInit = "";
      interactiveShellInit = ''
        source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
        source "${pkgs.autojump}/share/autojump/autojump.zsh"
        eval "$("${pkgs.direnv}/bin/direnv" hook zsh)"
      '';
    };
  };
  virtualisation = {
    docker = {
      enable = true;
      autoPrune.enable = true;
      storageDriver = "zfs";
    };
    libvirtd.enable = true;
  };

  # Packages
  environment.systemPackages = with pkgs; [
    # Graphical - Look & Feel
    adapta-backgrounds
    adapta-gtk-theme
    libsForQt5.qtstyleplugins # uniform QT/GTK look
    numix-icon-theme
    numix-icon-theme-circle
    xorg.xcursorthemes

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
    gimp
    gnome3.zenity
    kitty
    polybar
    virtmanager
    xfce.xfce4-power-manager

    # Command line
    borgbackup
    direnv
    rclone
    stow
    tmux

    # Must have utils
    ascii
    autojump
    bc
    bind
    binutils
    efibootmgr
    ffmpeg
    file
    gist
    git
    gnupg
    htop
    httpie
    manpages
    mosh
    ncdu
    ncftp
    neofetch
    nix-index
    patchelf
    pciutils
    rename
    socat
    trash-cli
    tree
    units
    unzip
    wget
    xclip
    xdotool
    youtube-dl
    zip

    # Daemons
    udiskie
  ];
}
