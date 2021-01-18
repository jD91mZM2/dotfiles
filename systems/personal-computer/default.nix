# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    # Flox :D
    (import (fetchTarball "https://github.com/flox/nixos-module/archive/master.tar.gz"))

    # This is my main computer, use all modules
    ../../templates/all.nix
  ];

  # Flox
  services.flox.substituterAdded = true;

  networking.hostName = "samuel-computer";
  networking.hostId = "c0122dbe";

  boot = {
    supportedFilesystems = [ "btrfs" "zfs" ];

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
  services.xserver = {
    videoDrivers = [ "amdgpu" ];
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --primary
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --set TearFree on
    '';
  };

  # Networking
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;

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
