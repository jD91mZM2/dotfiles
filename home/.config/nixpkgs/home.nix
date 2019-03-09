{ pkgs, lib, ... }:

let
  aliases = {
    clear = "clear; echo -ne \"\\e[3J\"";
    git = "hub";
    ls = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca = "rsync -avzhP";
  };
  bashConfig = builtins.replaceStrings [ "  " ] [ "\t" ] ''
      eval "$(dircolors "${dircolors}")"

      forward() {
        if [ -z "$1" ] || [ -z "$2" ]; then
          echo "forward <remote> <port>"
          return
        fi
        cat <<-EOF
        Remote port being forwarded over SSH!

        If it doesn't seem to work, make sure the remote's sshd_config
        specifies "GatewayPorts" to either "yes" or "clientspecified".
        EOF
        ssh "$1" -R ":''${2}:localhost:$2" -- sleep infinity
      }
  '';
  dircolors = pkgs.fetchFromGitHub {
    owner = "dotphiles";
    repo = "dotzsh";
    rev = "2.1.3";

    sha256 = "0h95s5gvn08m4y11gb82anx8s9s2ywaks15idawxdg5bibjav79l";
  } + "/themes/dotphiles/dircolors/dircolors.base16.dark";
in
{
  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };
  home.sessionVariables = import ./env.nix;
  home.packages = (import ./packages.nix { inherit pkgs lib; });
  nixpkgs.overlays = [
    (import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz))
  ];
  home.file.".xprofile".text = ''
    if [ -e ~/.profile ]; then
      source ~/.profile
    fi
  '';

  # CLI

  programs.ssh = {
    enable = true;
    matchBlocks = import ./ssh-hosts.nix;
  };

  programs.git = {
    enable = true;
    userName = "jD91mZM2";
    userEmail = "me@krake.one";

    signing = {
      key = "BC5DAE4EC168B1F9B94C98503055D54729A72666";
      signByDefault = true;
    };
    extraConfig = ''
      [pull]
      rebase = true;
      [diff]
      tool = nvimdifftool
      [difftool "nvimdifftool"]
      cmd = "nvim -d \"$LOCAL\" \"$REMOTE\""
      [merge]
      tool = nvimdifftool
      [mergetool "nvimdifftool"]
      cmd = "nvim -d \"$LOCAL\" \"$REMOTE\""
    '';
  };

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
        PS1="$(powerline-rs --shell zsh $?)"
      }
      precmd_functions+=(powerline)

      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
    '';
  };
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    defaultCacheTtlSsh = 86400;
    maxCacheTtl = 86400;
    maxCacheTtlSsh = 86400;
  };

  # Graphical

  xresources = {
    extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
      owner = "chriskempson";
      repo = "base16-xresources";
      rev = "79e6e1de591f7444793fd8ed38b67ce7fce25ab6";

      sha256 = "1nnj5py5n0m8rkq3ic01wzyzkgl3g9a8q5dc5pcgj3qr47hhddbw";
    } + "/xresources/base16-default-dark.Xresources");
    properties = {
      "Xcursor.theme" = "whiteglass";
      "Xcursor.size" = 16;

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
      name = "Numix-Circle";
      package = pkgs.numix-icon-theme-circle;
    };
    theme = {
      name = "Adapta-Nokto-Eta";
      package = pkgs.adapta-gtk-theme;
    };
  };

  services.compton = {
    enable = true;
    # See https://github.com/chjj/compton/issues/152
    backend = "xrender";

    fade = true;
    fadeDelta = 5;
    shadow = true;
    extraOptions = ''
      inactive-dim = 0.05;
      mark-ovredir-focused = false; # causes issues in xmonad
      focus-exclude = [ "class_g = 'xterm-256color'" ];
    '';
    opacityRule = [
      "90:class_g = 'xterm-256color'"
    ];
  };
  services.dunst = import ./dunst.nix { inherit pkgs; };
  services.udiskie.enable = true;
}
