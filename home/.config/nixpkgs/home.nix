{ pkgs, ... }:

let
  aliases = {
    clear = "clear; echo -ne \"\\e[3J\"";
    git = "hub";
    ls = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca = "rsync -avzhP";
  };
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
  home.sessionVariables = import ./profile.nix;
  home.packages = (import ./packages.nix { pkgs = pkgs; });

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
    profileExtra = ''
      # Use GNOME Keyring for ssh
      eval "$(gnome-keyring-daemon --start)"
      export SSH_AUTH_SOCK
    '';

    enable = true;
    shellAliases = aliases;
    initExtra = ''
      eval "$(dircolors "${dircolors}")"

      powerline() {
        PS1="$(powerline-rs --shell bash $?)"
        if [ -n "$IN_NIX_SHELL" ]; then
          PS1="''${PS1//\\\$/N}"
        fi
      }
      PROMPT_COMMAND=powerline
    '';
  };
  programs.zsh = {
    enable = true;
    shellAliases = aliases;
    initExtra = ''
      eval "$(dircolors "${dircolors}")"

      powerline() {
        PS1="$(powerline-rs --shell zsh $?)"
        if [ -n "$IN_NIX_SHELL" ]; then
          PS1="''${PS1//\%\#/N}"
        fi
      }
      precmd_functions+=(powerline)

      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
    '';
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
    fade = true;
    fadeDelta = 5;
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
}
