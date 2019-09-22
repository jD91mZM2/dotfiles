{ pkgs, lib, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};

  # Packages
  abottomod = shared.builders.buildPypiPackage {
    name = "abottomod";
    src = ~/Coding/Python/abottomod;
  };
in
{
  deployment = {
    targetEnv = "none";
    targetHost = "192.168.2.42";
  };

  nixpkgs.localSystem = (import <nixpkgs/lib>).systems.examples.aarch64-multiplatform;

  imports = [
    # Shared base settings
    ../base.nix
    ../syncthing.nix

    # Generated hardware configuration
    ./hardware-configuration.nix
  ];

  # Bootloader
  boot.loader.grub.enable = false;
  boot.loader.raspberryPi = {
    enable = true;
    uboot.enable = true;
    version = 3;
  };

  # Enable NetworkManager
  networking.wireless.enable = false; # disable default wireless support
  networking.networkmanager.enable = true;

  # Services
  custom.services = {
    abottomod.script = "${abottomod}/bin/start";
  };
  services.syncthing = {
    declarative.devices = shared.utils.without [ "rpi" ] shared.consts.syncthingDevices;
    relay = {
      enable = true;
      port = 443;
    };
  };
}
