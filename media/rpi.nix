{ pkgs, shared, ... }:
{
  imports = [
    # Base on top of the regular SD image
    <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-aarch64.nix>
  ];

  nixpkgs.localSystem = (import <nixpkgs/lib>).systems.examples.aarch64-multiplatform;

  i18n.consoleKeyMap = "dvorak";

  # Enable NetworkManager
  networking.wireless.enable = false; # disable default wireless support
  networking.networkmanager.enable = true;

  # Enable OpenSSH by default
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  users.users.root.openssh.authorizedKeys.keyFiles = shared.consts.sshKeys;
}
