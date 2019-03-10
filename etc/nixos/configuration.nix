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
    ./killswitch.nix
    ./openvpn.nix
    ./packages.nix
    ./services.nix
    ./x11.nix
  ];

  # File system
  services.zfs.autoSnapshot.enable = true;

  # systemd-boot
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [ "zfs" ];

  # TTY settings
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "dvorak";
    defaultLocale = "en_US.UTF-8";
  };

  # Time
  time.timeZone = "Europe/Stockholm";

  # Networking
  networking.hostId = "05fbf074";
  networking.hostName = "compotar";
  networking.networkmanager.enable = true;
  networking.nameservers = ["1.1.1.1" "1.0.0.1"];
  ## Manually overwrite /etc/resolv.conf because openresolv tries to add my ISP's DNS
  environment.etc."resolv.conf".text = ''
    ${builtins.concatStringsSep
        "\n"
        (map (ip: "nameserver " + ip) config.networking.nameservers)}
  '';

  # Mime type for wasm, see https://github.com/mdn/webassembly-examples/issues/5
  environment.etc."mime.types".text = ''
    application/wasm  wasm
  '';

  # User settings
  users.extraUsers.user = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker" "libvirtd" "adbusers"];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
