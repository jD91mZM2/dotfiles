# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ./env.nix
    ./fonts.nix
    ./packages.nix
    ./services.nix
    ./x11.nix
  ];

  # ZFS
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs = {
    autoSnapshot = {
      enable = true;

      # I never use these anyway, as everything I throw away goes in a
      # trashcan.
      frequent = 2;
      hourly = 5;
      daily = 3;
      weekly = 2;
      monthly = 0;
    };
    autoScrub.enable = true;
  };

  # Misc. settings
  documentation.dev.enable = true;
  hardware.bluetooth.enable = true;
  system.autoUpgrade.enable = true;
  time.timeZone = "Europe/Stockholm";

  # systemd-boot
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  # TTY settings
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "dvorak";
    defaultLocale = "en_US.UTF-8";
  };

  # Networking
  networking.hostId = "05fbf074";
  networking.hostName = "compotar";
  networking.networkmanager.enable = true;

  # Mime type for wasm, see https://github.com/mdn/webassembly-examples/issues/5
  environment.etc."mime.types".text = ''
    application/wasm  wasm
  '';

  # User settings
  users.extraUsers.user = {
    isNormalUser = true;
    extraGroups = ["wheel" "libvirtd" "adbusers"];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
