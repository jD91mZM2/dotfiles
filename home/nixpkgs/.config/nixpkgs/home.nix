{ pkgs, lib, ... }:

let
  aliases = {
    clear = "clear; echo -ne \"\\e[3J\"";
    e = "env -u TMPDIR emacsclient -n"; # nix-shell sets $TMPDIR which messes up emacsclient's search
    git = "hub";
    ls = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca = "rsync -avzhP --delete";
  };
  bashConfig = builtins.replaceStrings [ "  " ] [ "\t" ] ''
    eval "$(dircolors "${dircolors}")"
  '';
  dircolors = pkgs.fetchFromGitHub {
    owner = "dotphiles";
    repo = "dotzsh";
    rev = "2.1.3";

    sha256 = "0h95s5gvn08m4y11gb82anx8s9s2ywaks15idawxdg5bibjav79l";
  } + "/themes/dotphiles/dircolors/dircolors.base16.dark";

  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/release-19.03.tar.gz;
  };

  home.sessionVariables = import ./env.nix;
  home.packages = import ./packages.nix { inherit pkgs; };
  home.keyboard = null;

  home.file = {
    # Add kitty theme here, but don't add kitty config. Pinging
    # home-manager on every tiny config change isn't desirable here.
    ".config/kitty/theme.conf".source = (pkgs.fetchFromGitHub {
      owner = "kdrag0n";
      repo = "base16-kitty";
      rev = "858b3e36549e0415623218caa6f0a8d7a1f5edab";
      sha256 = "0x449q9b75fql1hp9ryak7jd63x47480x1k9fgvasdgg0bpdm03k";
    }) + "/colors/base16-tomorrow-night.conf";

    "Pictures/Backgrounds/background.jpg".source = pkgs.my-background;
    "Pictures/Backgrounds/background-focus.jpg".source = pkgs.my-background-focus;
  };

  #   ____ _     ___
  #  / ___| |   |_ _|
  # | |   | |    | |
  # | |___| |___ | |
  #  \____|_____|___|

  # Shells
  programs.bash = {
    enable = true;
    shellAliases = aliases;
    initExtra = ''
      ${bashConfig}

      powerline() {
        PS1="$(powerline-rs --shell bash $?)"
      }
      PROMPT_COMMAND=powerline
    '';
  };
  programs.zsh = {
    enable = true;
    shellAliases = aliases;
    initExtra = ''
      ${bashConfig}

      powerline() {
        local exit_code="$?"
        if [[ "$TERM" == eterm* ]]; then
            PS1="''${PWD/$HOME/~} %% "
        else
            PS1="$(powerline-rs --shell zsh "$exit_code")"
        fi
      }
      precmd_functions+=(powerline)

      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
    '';
  };

  # Misc
  programs.ssh = {
    enable = true;
    matchBlocks = shared.consts.secret.sshHosts;
  };
  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = shared.consts.name;
    userEmail = shared.consts.email;

    signing = {
      key = "BC5DAE4EC168B1F9B94C98503055D54729A72666";
      signByDefault = true;
    };
    extraConfig = {
      pull.rebase = true;
    } // shared.consts.secret.gitConfig;
  };

  # Services
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    defaultCacheTtlSsh = 86400;
    maxCacheTtl = 86400;
    maxCacheTtlSsh = 86400;
  };

  #   ____                 _     _           _
  #  / ___|_ __ __ _ _ __ | |__ (_) ___ __ _| |
  # | |  _| '__/ _` | '_ \| '_ \| |/ __/ _` | |
  # | |_| | | | (_| | |_) | | | | | (_| (_| | |
  #  \____|_|  \__,_| .__/|_| |_|_|\___\__,_|_|
  #                 |_|

  # Configs
  xsession = {
    enable = true;
    pointerCursor = {
      package = pkgs.xorg.xcursorthemes;
      name = "whiteglass";
      size = 16;
    };
    windowManager.command = ''
      ${pkgs.sxhkd}/bin/sxhkd &
      ${pkgs.bspwm}/bin/bspwm
    '';
  };
  xresources = {
    extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
      owner = "chriskempson";
      repo = "base16-xresources";
      rev = "79e6e1de591f7444793fd8ed38b67ce7fce25ab6";

      sha256 = "1nnj5py5n0m8rkq3ic01wzyzkgl3g9a8q5dc5pcgj3qr47hhddbw";
    } + "/xresources/base16-default-dark.Xresources");
    properties = {
      "XTerm.termName" = "xterm-256color";
      "XTerm.vt100.faceName" = "Hack:size=10";

      # Sixel stuff
      "XTerm*decTerminalID" = "vt340";
      "XTerm*numColorRegisters" = 256;
    };
  };
  gtk = {
    enable = true;
    font = {
      name = "Cantarell 11";
      package = pkgs.cantarell-fonts;
    };
    iconTheme = {
      name = "Yaru";
      package = pkgs.yaru-theme-latest;
    };
    theme = {
      name = "Yaru-dark";
      package = pkgs.yaru-theme-latest;
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  # Services
  services.compton = {
    enable = true;
    # Apparently, using (intel + xrandr to configure multiple monitors + glx
    # backend) seems to cause all kinds of weird issues.
    backend = "xrender";

    fade = true;
    fadeDelta = 5;
    shadow = true;
    extraOptions = ''
      inactive-dim = 0.1;
    '';
  };
  services.dunst = import ./dunst.nix { inherit pkgs; };
  services.udiskie.enable = true;
}
