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
    go
    gotools
    just
    olive-editor # <- THIS IS AMAZING
    superTuxKart
  ])

  ++

  # Stable packages
  (with pkgs; [
    # My software
    powerline-rs
    termplay
    xidlehook
    # Other local overlays
    clangd

    # Graphical applications
    abiword
    chromium
    gimp
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

    # Must have utils
    ascii
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
    nix-review
    nixops
    pv
    rclone
    rename
    ripgrep
    sqlite
    weechat
    youtube-dl
    tree
    units

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
    (python3.withPackages (p: with p; [
      python-language-server
      tkinter
    ]))
    ruby
    rustup
    sbcl

    # LaTeX stuff
    okular
    poppler_utils
    texlive.combined.scheme-full
  ])
