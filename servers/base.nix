{ pkgs, ... }:

let
  shared = import <dotfiles/shared> {};
in {
  imports = [
    (import <dotfiles/nur-packages> {}).modules.programs
    <dotfiles/shared/base.nix>
  ];

  # Temporary: Hack around flakes
  _module.args = {
    inherit shared;
    self = import <dotfiles>;
  };

  # Language settings
  console.keyMap = "dvorak";

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

  # Add SSH key to user account
  users.users."${shared.consts.user}".openssh.authorizedKeys.keys = shared.consts.sshKeys;

  # Extra packages
  environment.systemPackages = with pkgs; [
    rclone
    sqlite
  ];

  # User settings
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Program config
  programs.mosh.enable = true;
}
