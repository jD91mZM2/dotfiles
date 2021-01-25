{ config, pkgs, ... }:

{
  imports = [

    # Base modules
    ../../modules/base
    ../../modules/meta.nix
    ../../modules/user

    # CLI packages
    ../modules/packages/neovim.nix

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
  users.users."${config.userName}".openssh.authorizedKeys.keys = config.globals.sshKey;

  # Install mosh for OpenSSH
  programs.mosh.enable = true;

  # User settings
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Don't run out of space
  nix.gc = {
    automatic = true;
    options = "-d";
  };
  nix.optimise.automatic = true;
}
