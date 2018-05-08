{ pkgs, ... }:

let
  aliases = {
    clear = "clear; echo -ne \"\\e[3J\"";
    git = "hub";
    ls = "ls -CF --color=auto";
    nix-shell = "nix-shell --run zsh";
    rsynca = "rsync -avzhP";
  };
in
{
  # Imports
  services.dunst = (import ./dunst.nix { inherit pkgs; });

  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };
  home.packages = with pkgs; [
    chromium
    discord-latest
    dropbox-cli
    liferea
    thunderbird
  ];

  # CLI

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
    profileExtra = (builtins.readFile ./profile);
    initExtra = ''
      eval "$(dircolors ~/.dircolors)"

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
      eval "$(dircolors ~/.dircolors)"

      powerline() {
          PS1="$(powerline-rs --shell zsh $?)"
      }
      precmd_functions+=(powerline)

      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
    '';
  };

  # Graphical

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
    '';
  };
}
