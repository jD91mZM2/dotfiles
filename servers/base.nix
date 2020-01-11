{ pkgs, ... }:
let
  shared = pkgs.callPackage <dotfiles/shared> {};
  nur-no-pkgs = import <dotfiles/shared/nur-no-pkgs.nix>;
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

  # Language settings
  i18n.consoleKeyMap = "dvorak";
  services.xserver.layout = "dvorak";

  # System config
  networking.firewall.enable = false;
  nix.gc = {
    automatic = true;
    dates     = "monthly";
    options   = "-d";
  };

  # OpenSSH settings
  services.openssh = {
    enable                 = true;
    forwardX11             = true;
    gatewayPorts           = "clientspecified";
    passwordAuthentication = false;
  };

  # User settings
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  users.defaultUserShell = pkgs.zsh;

  # Default user
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = shared.consts.sshKeys;
  };

  # Default packages
  environment.systemPackages = with pkgs; [
    file
    htop
    kitty.terminfo
    rclone
    sqlite
    trash-cli
    tree
  ];

  # Program config
  programs.powerline-rs.enable = true;
  programs.mosh.enable = true;
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    interactiveShellInit = ''
      . ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';
    syntaxHighlighting.enable = true;
  };
}
