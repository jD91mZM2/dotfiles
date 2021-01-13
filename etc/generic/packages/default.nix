{ config, pkgs, lib, ... }:

{
  imports = [
    ./graphics.nix
    ./languages.nix
    ./scripts.nix
    ./shells.nix
  ];

  config = {
    # More involved programs
    programs = {
      adb.enable = true;
      dconf.enable = true;
      slock.enable = true;
    };
    virtualisation = {
      # Podman > Docker, change my mind
      podman.enable = true;
      # Libvirt VMs
      libvirtd.enable = true;
    };

    environment.systemPackages = with pkgs; [
      # Applications
      autojump
      direnv
      neovim
      tmux

      # Must have utils
      bc
      bind
      binutils
      cryptsetup
      efibootmgr
      file
      gist
      git
      gnupg
      htop
      httpie
      manpages
      mosh
      ncdu
      nethogs
      nix-index
      patchelf
      pciutils
      socat
      trash-cli
      tree
      unzip
      wget
      zip
    ];

    # Most of my installed applications are nowadays installed through
    # here. The line between what I want as a "system package" and a "user
    # package" is incredibly blurry, but I am currently going with the
    # following definition:

    # Can I live without it temporarily, should I need to debug my user
    # config or log in as a different user?
    # Answer is "preferrably no"? It's a system package.
    setup.home.modules = lib.singleton {
      home.packages = with pkgs; [
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
        graphviz
        imagemagick
        mkchromecast
        pandoc
        termplay
        translate-shell
        whois
        wildmidi
        (runCommand "mgitstatus" { } ''
          cd "${fetchFromGitHub {
            owner = "fboender";
            repo = "multi-git-status";
            rev = "0f67ed0de0417823e52a2c4be2eea26208c0647c";
            sha256 = "1i4nm842bv0clkwm46553h6y0bg61xm5g54w28qfjxylzxjjmwhm";
          }}"
          mkdir "$out"
          PREFIX="$out" ./install.sh
        '')

        # Nix stuff
        cachix
        nix-prefetch-scripts # - broken. todo
        nix-review
        nixos-generators
        rnix-lsp
      ];
    };
  };
}
