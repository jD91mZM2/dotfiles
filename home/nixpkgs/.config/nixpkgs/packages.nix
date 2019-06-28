{ pkgs }:

# Most of my installed applications are nowadays installed through
# here. The line between what I want as a "system package" and a "user
# package" is incredibly blurry, but I am currently going with the
# following definition:

# Can I live without it temporarily, should I need to debug my user
# config or log in as a different user?
# Answer is "preferrably no"? It's a system package.

let
  unstable = import <nixos-unstable> {};
in
  # Unstable packages
  (with unstable; [
    firefox
    superTuxKart
  ])

  ++

  # Stable packages
  (with pkgs; [
    # My software
    powerline-rs
    termplay
    xidlehook

    # Graphical applications
    abiword
    chromium
    inkscape
    keepassxc
    liferea
    maim
    mpv
    multimc
    musescore
    obs-studio
    pavucontrol
    thunderbird
    tigervnc
    torbrowser
    xorg.xev
    xorg.xwininfo

    # Command line applications
    asciinema
    cdrkit
    ctags
    docker_compose
    fd
    ffmpeg
    figlet
    gitAndTools.hub
    ncftp
    neofetch
    nixops
    ripgrep
    sqlite
    weechat

    # Languages
    cabal-install
    cargo-edit
    cargo-release
    cargo-tree
    carnix
    cmake
    gcc
    gdb
    ghc
    gnumake
    pypi2nix
    python3
    python36Packages.python-language-server
    ruby
    rustup

    # LaTeX stuff
    okular
    poppler_utils
    texlive.combined.scheme-full
  ])
