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

    ../../modules/base
    ../../modules/console.nix
    ../../modules/efi-boot.nix
    ../../modules/gpg.nix
    ../../modules/meta.nix
    ../../modules/neovim.nix
    ../../modules/user
    ../../modules/x11
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

  # Add some extra drivers
  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };

  services.xserver = {
    videoDrivers = [ "amdgpu" ];
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --primary
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --set TearFree on
    '';
  };

  # TODO:
  #
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  #
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.interfaces.virbr0.useDHCP = true;
  networking.interfaces.virbr0-nic.useDHCP = true;

  # -------------------

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   wget vim
  #   firefox
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?

}
