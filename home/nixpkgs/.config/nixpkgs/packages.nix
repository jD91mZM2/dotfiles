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
  with pkgs; [
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
    superTuxKart
    thunderbird
    torbrowser
    xorg.xev
    xorg.xwininfo

    # Command line applications
    asciinema
    cdrkit
    docker_compose
    fd
    figlet
    gitAndTools.hub
    nixops
    ripgrep
    sqlite
    weechat

    # Languages
    cabal-install
    cargo-edit
    cargo-release
    cargo-tree
    cmake
    gcc
    gdb
    ghc
    gnumake
    pypi2nix
    ruby
    rustup
    unstable.carnix
    unstable.python3
    #unstable.python36Packages.python-language-server
    python36Packages.python-language-server

    # LaTeX stuff
    okular
    poppler_utils
    texlive.combined.scheme-full
  ]
