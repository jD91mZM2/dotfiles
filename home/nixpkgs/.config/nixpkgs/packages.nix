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

  reminder = ''

      If it doesn't seem to work, make sure the remote's sshd_config
      specifies "GatewayPorts" to either "yes" or "clientspecified".
  '';
  forward = let
    usage = "forward <remote> <port>";
  in pkgs.writeShellScriptBin "forward" ''
      : "''${1:?${usage}}"
      : "''${2:?${usage}}"
      cat <<-EOF
      Remote port being forwarded over SSH!
      ${reminder}
      EOF

      ssh "$1" -R ":''${2}:localhost:$2" -- sleep infinity
  '';
  backward = let
    usage = "backward <remote> <port>";
  in pkgs.writeShellScriptBin "backward" ''
      : "''${1:?${usage}}"
      : "''${2:?${usage}}"
      cat <<-EOF
      Local port being forwarded to a remote application over SSH!
      ${reminder}
      EOF

      ssh "$1" -L ":''${2}:localhost:$2" -- sleep infinity
  '';
in
  # Convenient bash aliases
  [
    forward
    backward
  ]

  ++

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
