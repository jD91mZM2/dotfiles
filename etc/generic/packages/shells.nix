{ config, pkgs, lib, shared, inputs, system, ... }:

{
  users.defaultUserShell = inputs.nix-exprs.packages."${system}".zsh;
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
  };
}
