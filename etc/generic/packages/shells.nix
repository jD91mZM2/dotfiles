{ config, pkgs, lib, shared, ... }:

{
  users.defaultUserShell = pkgs.zsh;
  programs = {
    powerline-rs = {
      enable = true;
      args = [ "--cwd-max-depth" "3" ];
    };
    bash = {
      enableCompletion = true;
      interactiveShellInit = ''
        source "${pkgs.autojump}/share/autojump/autojump.bash"
        eval "$("${pkgs.direnv}/bin/direnv" hook bash)"
      '';
    };
    zsh = {
      enable = true;
      autosuggestions = {
        enable = true;
        highlightStyle = "bg=#${(shared.theme.getColor 1).rgb},fg=#${(shared.theme.getColor 3).rgb}";
      };
      syntaxHighlighting.enable = true;
      vi.enable = true;
      interactiveShellInit = ''
        source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
        source "${pkgs.autojump}/share/autojump/autojump.zsh"
        eval "$("${pkgs.direnv}/bin/direnv" hook zsh)"

        # Free up the '#' symbol, for use in nix flakes
        unsetopt extendedglob
      '';
    };
  };
}
