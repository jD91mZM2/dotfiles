# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, options, pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  options.setup = {
    name = lib.mkOption {
      type = lib.types.str;
      description = "The name of this deployment, same as the folder's name in /etc.";
    };
    networkId = lib.mkOption {
      type = lib.types.str;
      description = "Same as network.hostId, obtain using `head -c8 /etc/machine-id`";
    };
  };

  imports = [
    # Files
    <dotfiles/shared/base.nix>
    ./cachix.nix
    ./containers.nix
    ./fonts.nix
    ./packages.nix
    ./services.nix
    ./sudo.nix
    ./x11.nix
  ];

  config = {
    nix.nixPath = [
      "dotfiles=${shared.consts.dotfiles}"
      "nixos-config=${shared.consts.dotfiles}/etc/${config.setup.name}/configuration.nix"
    ] ++ (lib.filter (key: !(lib.hasPrefix "nixos-config=" key)) options.nix.nixPath.default);

    boot.supportedFilesystems = [ "btrfs" "zfs" ];

    # These systems will be able to be emulated transparently. Enabling
    # aarch64 will allow me to run aarch64 executables (using
    # qemu-aarch64 behind the scenes). If I were to enable windows here,
    # all .exe files will be handled using WINE.
    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

    # Use latest kernel, and some extra drivers
    boot.kernelPackages = pkgs.linuxPackages_latest;
    hardware.enableRedistributableFirmware = true;

    # Misc. settings
    documentation.dev.enable  = true;
    hardware.bluetooth.enable = true;
    nix.gc = {
      automatic = true;
      dates = "17:00";
      options = "--delete-older-than 10d";
    };
    nix.optimise = {
      automatic = true;
      dates = "17:00";
    };
    system.autoUpgrade = {
      enable = true;
      dates  = "16:00";
    };
    time = {
      timeZone = "Europe/Stockholm";
      hardwareClockInLocalTime = true; # fuck windows
    };

    # systemd-boot
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.systemd-boot = {
      enable = true;
      configurationLimit = 5;
      editor = false;
    };

    # TTY settings
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font   = "Lat2-Terminus16";
      keyMap = "dvorak";
    };

    # Networking
    networking.hostId = config.setup.networkId;
    networking.hostName = "samuel-${config.setup.name}";
    networking.networkmanager.enable = true;

    # Mime type for wasm, see https://github.com/mdn/webassembly-examples/issues/5
    environment.etc."mime.types".text = ''
      application/wasm  wasm
    '';

    # User settings
    users.users."${shared.consts.user}" = {
      isNormalUser = true;
      extraGroups  = [ "libvirtd" "adbusers" ];
    };

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "18.03"; # Did you read the comment?
  };
}
