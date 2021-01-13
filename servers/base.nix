{ system, shared, pkgs, ... }:

{
  imports = [
    ./etc/generic
  ];

  # System config
  networking.firewall.enable = false;

  # Don't run out of space
  nix.gc = {
    automatic = true;
    options   = "-d";
  };
  nix.optimise.automatic = true;

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
