{ pkgs, lib, ... }:

let
  aliases = {
    clear     = "clear; echo -ne \"\\e[3J\"";
    e         = "env -u TMPDIR emacsclient -n"; # nix-shell sets $TMPDIR which messes up emacsclient's search
    git       = "hub";
    ls        = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca    = "rsync -avzhP --delete";
  };
  bashConfig = builtins.replaceStrings [ "  " ] [ "\t" ] ''
    eval "$(dircolors "${dircolors}")"
  '';

  dircolors = pkgs.fetchFromGitHub {
    owner  = "joshbenham";
    repo   = "linux-dotfiles";
    rev    = "67641154e7befa67527f73a6cbf64b36e15641ca";

    sha256 = "0hvnbc2wlx6j0p4k1znx72ma9rnvf55b9mcfays3pdn80qsx9s8q";
  } + "/dircolors/Dracula.dircolors";

  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  imports = [
    ./dunst.nix
    ./env.nix
    ./keyring.nix
    ./packages.nix
    ./polybar.nix
  ];

  programs.home-manager = {
    enable = true;
    path   = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };

  home.keyboard = null;

  home.file = {
    # Add kitty theme here, but don't add kitty config. Pinging
    # home-manager on every tiny config change isn't desirable here.
    ".config/kitty/theme.conf".source = (pkgs.fetchFromGitHub {
      owner  = "dracula";
      repo   = "kitty";
      rev    = "5986829ce6897f0775529b6fbe9169f909ef209f";

      sha256 = "0xv8klvrwd68k589i4kihdl3mkgkaflh7j6iaxig6p52rw18636y";
    }) + "/dracula.conf";

    "Pictures/Backgrounds/background.jpg".source = pkgs.background;
    "Pictures/Backgrounds/background-focus.jpg".source = pkgs.background-focus;
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

      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=#${shared.theme.current-line},fg=#${shared.theme.comment}"
    '';
  };

  # Misc
  programs.ssh = {
    enable      = true;
    matchBlocks = shared.consts.secret.sshHosts;
  };
  programs.git = {
    enable     = true;
    lfs.enable = true;
    userName   = shared.consts.name;
    userEmail  = shared.consts.email;

    signing = {
      key = shared.consts.gpgKeys.signing;
      signByDefault = true;
    };
    extraConfig = {
      pull.rebase = true;
    } // shared.consts.secret.gitConfig;
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
      name    = "whiteglass";
      size    = 16;
    };
    windowManager.command = ''
      ${pkgs.sxhkd}/bin/sxhkd &
      ${pkgs.bspwm}/bin/bspwm
    '';
  };
  xresources = {
    # (...)s around the expression just for my editor's sake.
    extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
      owner  = "dracula";
      repo   = "xresources";
      rev    = "ca0d05cf2b7e5c37104c6ad1a3f5378b72c705db";

      sha256 = "0ywkf2bzxkr45a0nmrmb2j3pp7igx6qvq6ar0kk7d5wigmkr9m5n";
    } + "/Xresources");
    properties = {
      "XTerm.termName"          = "xterm-256color";
      "XTerm.vt100.faceName"    = "Hack:size=10";

      # Sixel stuff
      "XTerm*decTerminalID"     = "vt340";
      "XTerm*numColorRegisters" = 256;
    };
  };
  gtk = {
    enable = true;
    font = {
      name    = "Cantarell 11";
      package = pkgs.cantarell-fonts;
    };
    iconTheme = {
      name    = "Yaru";
      package = pkgs.yaru-dracula-theme;
    };
    theme = {
      name    = "Yaru-dark";
      package = pkgs.yaru-dracula-theme;
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
}
