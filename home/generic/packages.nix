{ pkgs, ... }:

# Most of my installed applications are nowadays installed through
# here. The line between what I want as a "system package" and a "user
# package" is incredibly blurry, but I am currently going with the
# following definition:

# Can I live without it temporarily, should I need to debug my user
# config or log in as a different user?
# Answer is "preferrably no"? It's a system package.

let
  script = name: message: arg: let
    usage = "${name} <remote> <ports...>";
  in
    pkgs.writeShellScriptBin name ''
      remote="''${1:?${usage}}"
      : "''${2:?${usage}}"
      cat <<-EOF
      ${message}

      If it doesn't seem to work, make sure the remote's sshd_config
      specifies "GatewayPorts" to either "yes" or "clientspecified".
      EOF

      # Shift away first argument
      shift

      arguments=()
      for port in "$@"; do
        arguments+=(${arg} ":''${port}:localhost:''${port}")
      done

      set -x
      ssh "$remote" "''${arguments[@]}" -- sleep infinity
    '';
  forward = script "forward" "Remote port being forwarded over SSH!" "-R";
  backward = script "backward" "Local port being forwarded to a remote application over SSH!" "-L";

  my-preferred-java-version = pkgs.openjdk8;
in {
  home.sessionVariables.JAVA_HOME = "${my-preferred-java-version.home}";
  home.packages =
    # Convenient bash aliases
    [
      forward
      backward
    ]

    ++

    # Stable packages
    (with pkgs; [
      # Local overlays
      clangd

      # My software
      rnix-lsp
      termplay

      # Graphical applications
      (callPackage <dotfiles/forks/st> {})
      abiword
      audacity
      bitwarden
      chromium
      filezilla
      firefox
      gimp
      inkscape
      keepassxc
      mpv
      mullvad-vpn
      multimc
      musescore
      obs-studio
      olive-editor # <- THIS IS AMAZING
      pavucontrol
      radare2-cutter
      scrcpy
      superTuxKart
      thunderbird
      xorg.xev
      xorg.xwininfo

      # Must have utils
      ascii
      ctags
      fd
      ffmpeg
      figlet
      libnotify
      neofetch
      pv
      rclone
      rename
      ripgrep
      sqlite
      weechat
      youtube-dl

      # Other CLI utils
      asciinema
      cdrkit
      docker_compose
      graphviz
      imagemagick
      pandoc
      translate-shell
      whois
      wildmidi
      (runCommand "mgitstatus" {} ''
        cd "${fetchFromGitHub {
          owner = "fboender";
          repo = "multi-git-status";
          rev = "0f67ed0de0417823e52a2c4be2eea26208c0647c";
          sha256 = "1i4nm842bv0clkwm46553h6y0bg61xm5g54w28qfjxylzxjjmwhm";
        }}"
        mkdir "$out"
        PREFIX="$out" ./install.sh
      '')

      # Desktop items
      (makeDesktopItem {
        name = "discord";
        desktopName = "Discord";
        exec = "${chromium}/bin/chromium --app=https://discordapp.com/channels/@me";
      })
      (makeDesktopItem {
        name = "redox-mattermost";
        desktopName = "Redox Mattermost";
        exec = "${chromium}/bin/chromium --app=https://chat.redox-os.org/";
      })

      # Nix stuff
      (callPackage (builtins.fetchTarball https://cachix.org/api/v1/install) {}).cachix
      (callPackage (builtins.fetchTarball https://github.com/kolloch/crate2nix/archive/master.tar.gz) {})
      (callPackage (builtins.fetchTarball https://github.com/nix-community/pypi2nix/archive/master.tar.gz) {})
      niv
      nix-prefetch-scripts
      nix-review

      # Languages
      ## Haskell
      stack
      ## Rust
      cargo-edit
      cargo-release
      cargo-tree
      rust-analyzer
      rustup
      ## C
      cmake
      gcc
      gdb
      gnumake
      ## Java & JVM languages :puke:
      my-preferred-java-version
      jetbrains.idea-community
      ## Go
      go
      gopls
      ## Markdown
      mdl
      ## Python
      (python3.withPackages (p: with p; [
        # python-language-server - broken?
        tkinter
      ]))
      ## LaTeX
      zathura
      poppler_utils
      texlive.combined.scheme-full
      ## Web Assembly
      wabt
    ]);
}
