# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ options, pkgs, lib, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  disabledModules = [ "services/networking/syncthing.nix" ];
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    # Files
    ./fonts.nix
    ./packages.nix
    ./services.nix
    ./sudo.nix
    ./x11.nix

    # Unstable modules
    <nixos-unstable/nixos/modules/services/networking/syncthing.nix>

    # External modules
    (builtins.fetchGit {
      url = "https://github.com/cleverca22/nixos-configs";
      rev = "76260ad60cd99d40ab25df1400b0663d48e736db";
      ref = "master";
    } + "/qemu.nix")
  ];

  # Thanks, @clever! /blob/76260ad60cd99d40ab25df1400b0663d48e736db/qemu.nix
  qemu-user.aarch64 = true;

  nix.nixPath = [
    "dotfiles=${shared.consts.dotfiles}"
    "nixos-config=${shared.consts.dotfiles}/etc/nixos/configuration.nix"
  ] ++ (lib.filter (key: !(lib.hasPrefix "nixos-config=" key)) options.nix.nixPath.default);

  boot.supportedFilesystems = [ "zfs" ];

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
  users.users."${shared.consts.user}" = {
    isNormalUser = true;
    extraGroups = [ "wheel" "libvirtd" "adbusers" ];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
