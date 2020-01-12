{ pkgs, ... }:

{
  imports = [
    <dotfiles/shared/base.nix>
  ];
  # Language settings
  i18n.consoleKeyMap = "dvorak";

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

  environment.systemPackages = [
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
