{ pkgs, config, ... }:
let
  getColour = builtins.elemAt config.globals.colourscheme.colours;
in
{
  # Set zsh as default shell
  users.defaultUserShell = pkgs.zsh;

  programs = {
    # Shell prompt
    powerline-rs.enable = true;

    # Zsh config
    zsh = {
      autosuggestions = {
        enable = true;
        highlightStyle = "bg=#${getColour 1},fg=#${getColour 3}";
      };
      syntaxHighlighting.enable = true;

      # Vi-like editing
      vi.enable = true;

      interactiveShellInit = ''
        # Load plugins
        source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"

        unset -f trans # some alias by grml-zsh-config

        # Free up the '#' symbol, for use in nix flakes
        unsetopt extendedglob
      '';
    };
  };

  environment.shellAliases = {
    # Override program settings
    cal = "cal -m";
    clear = "clear; echo -ne \"\\e[3J\"";
    nix-shell = "nix-shell --command zsh";
    objdump = "objdump -Mintel";

    # Override programs
    cat = "bat";

    # Override ls
    ls = "exa";
    ll = "exa --git -l";
    la = "exa --git -laag";
  };

  home = {
    programs.zsh = {
      # Don't have zsh prompting us to create a .zshrc
      enable = true;

      # Move .zshrc out of home dir
      dotDir = ".config/zsh";
    };

    # Faster navigation between directories
    programs.autojump.enable = true;

    # Automatically load project-specific environments
    programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };
  };
}
