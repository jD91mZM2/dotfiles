{ pkgs }:

# Most of my installed applications are nowadays installed through
# here. The line between what I want as a "system package" and a "user
# package" is incredibly blurry, but I am currently going with the
# following definition:

# Can I live without it temporarily, should I need to debug my user
# config or log in as a different user?
# Answer is "preferrably no"? It's a system package.

let
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
  (with pkgs.unstable; [
  ])

  ++

  # Stable packages
  (with pkgs; [
    # Local overlays
    clangd

    # My software
    termplay
    unmerged.git-subcopy
    unmerged.powerline-rs
    unmerged.scaff
    (callPackage (builtins.fetchTarball https://gitlab.com/jD91mZM2/xidlehook/-/archive/master.tar.gz) {})

    # Graphical applications
    abiword
    chromium
    firefox
    gimp
    inkscape
    keepassxc
    liferea
    mpv
    multimc
    musescore
    obs-studio
    olive-editor # <- THIS IS AMAZING
    pavucontrol
    superTuxKart
    thunderbird
    tigervnc
    torbrowser
    xorg.xev
    xorg.xwininfo

    # Must have utils
    ascii
    ctags
    fd
    ffmpeg
    figlet
    gitAndTools.hub
    ncftp
    neofetch
    nixops
    pv
    rclone
    rename
    ripgrep
    sqlite
    tree
    units
    weechat
    youtube-dl

    # Other CLI utils
    asciinema
    cdrkit
    docker_compose
    imagemagick
    pandoc
    whois

    # Nix stuff
    (callPackage (builtins.fetchTarball https://cachix.org/api/v1/install) {}).cachix
    (callPackage (builtins.fetchTarball https://github.com/kolloch/crate2nix/archive/master.tar.gz) {})
    (callPackage (builtins.fetchTarball https://github.com/nix-community/pypi2nix/archive/db43e3b896739c78bd1abed1ba45c52d1bc74c7e.tar.gz) {})
    nix-prefetch-scripts
    nix-review

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
    go
    gotools
    mdl
    multimarkdown
    nodejs
    (python3.withPackages (p: with p; [
      python-language-server
      tkinter
    ]))
    # ruby
    rustup
    sbcl

    # LaTeX stuff
    okular
    poppler_utils
    texlive.combined.scheme-full
  ])
