{ config, pkgs, lib, ... }:

with lib;
{
  imports = [

    ./secrets.nix

    # Base modules
    ../../modules/base
    ../../modules/meta.nix
    ../../modules/user

    # CLI packages
    ../../modules/packages/neovim.nix

  ];

  # Allow all incoming connections to be handled by respective programs
  networking.firewall.enable = false;

  # OpenSSH settings
  services.openssh = {
    enable = true;
    forwardX11 = true;
    gatewayPorts = "clientspecified";
    passwordAuthentication = false;
  };

  # Add SSH key to accounts
  users.users = {
    root.openssh.authorizedKeys.keys = singleton config.globals.sshKey;
    "${config.globals.userName}".openssh.authorizedKeys.keys = singleton config.globals.sshKey;
  };

  # Install mosh for OpenSSH
  programs.mosh.enable = true;

  # User settings
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Don't run out of space - override my default options
  nix.gc = {
    automatic = true;
    options = mkForce "-d";
  };
}
