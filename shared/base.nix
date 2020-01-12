{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  nur-no-pkgs = import ./nur-no-pkgs.nix;
in
{
  imports = [
    nur-no-pkgs.repos.jd91mzm2.modules.programs
  ];

  # System overlays
  nixpkgs.overlays = let
    dir = (<dotfiles/home/nixpkgs/.config/nixpkgs/overlays>);
    names = builtins.attrNames (builtins.readDir dir);
  in
    (map (name: import (dir + "/${name}")) names);

  # Must have packages
  environment.systemPackages = with pkgs; [
    file
    htop
    kitty.terminfo
    trash-cli
    tree
  ];

  # Programs
  programs = {
    powerline-rs.enable = true;
    bash = {
      enableCompletion = true;
      interactiveShellInit = ''
        source "${pkgs.autojump}/share/autojump/autojump.bash"
        eval "$("${pkgs.direnv}/bin/direnv" hook bash)"
      '';
    };
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      interactiveShellInit = ''
        source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
        source "${pkgs.autojump}/share/autojump/autojump.zsh"
        eval "$("${pkgs.direnv}/bin/direnv" hook zsh)"

        set noextendglob
      '';
      syntaxHighlighting.enable = true;
    };
  };

  # Default user
  users = {
    defaultUserShell = pkgs.zsh;
    users."${shared.consts.user}" = {
      # This is a hardcoded uid, used by any container. This means
      # files shared with containers are accessible.
      uid = 1000;

      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
  };
}
