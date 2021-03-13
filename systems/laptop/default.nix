# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

with lib;

let
  home = "/home/${config.globals.userName}";
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    # This is my personal laptop, use all modules
    ../../templates/all.nix

    # Use ZFS filesystem
    ../../modules/zfs.nix
  ];

  boot = {
    supportedFilesystems = [ "btrfs" ];

    # Make timeout long so I have time to plug in my keyboard. Using `null` here
    # seems to cause it to be selected immediately, unlike what the man page says
    loader.timeout = 99;
  };

  # Add some extra kernel drivers
  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };

  # Video Drivers
  services.xserver.videoDrivers = [ "intel" ];

  # Networking
  networking = {
    hostName = "samael-laptop";
    hostId = "e345d278";

    networkmanager.enable = true;

    useDHCP = false;
    # interfaces = {
    #   enp2s0f1.useDHCP = true;
    #   wlp3s0.useDHCP = true;
    # };

    # Allow all local devices to access my open ports :)
    firewall.enable = false;
  };
  users.users."${config.globals.userName}".extraGroups = [ "networkmanager" ];
  home.services.network-manager-applet.enable = true;

  # Syncthing
  services.syncthing.declarative = {
    cert = "${home}/Sync/secrets/syncthing/laptop/cert.pem";
    key = "${home}/Sync/secrets/syncthing/laptop/key.pem";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?

  # Same, but for home-manager
  home.home.stateVersion = "21.03";
}
