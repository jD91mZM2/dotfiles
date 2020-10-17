# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, options, pkgs, shared, ... }:

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
    ./cachix.nix
    ./containers.nix
    ./fonts.nix
    ./packages.nix
    ./services.nix
    ./sudo.nix
    ./x11.nix
  ];

  config = {
    nix = {
      # Nix flakes
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';

      # Input Output HK - Hydra cache for haskell.nix
      binaryCachePublicKeys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      binaryCaches = [
        "https://hydra.iohk.io"
      ];

      # Add dotfiles to my nix path
      nixPath = [
        "dotfiles=${shared.consts.dotfiles}"
        "nixos-config=${shared.consts.dotfiles}/etc/${config.setup.name}/configuration.nix"
      ] ++ (lib.filter (key: !(lib.hasPrefix "nixos-config=" key)) options.nix.nixPath.default);

      # Keep my harddrive relatively small
      gc = {
        automatic = true;
        dates = "17:00";
        options = "--delete-older-than 10d";
      };
      optimise = {
        automatic = true;
        dates = [ "17:00" ];
      };
    };

    boot = {
      supportedFilesystems = [ "btrfs" "zfs" ];

      # These systems will be able to be emulated transparently. Enabling
      # aarch64 will allow me to run aarch64 executables (using
      # qemu-aarch64 behind the scenes). If I were to enable windows here,
      # all .exe files will be handled using WINE.
      binfmt.emulatedSystems = [ "aarch64-linux" ];

      # Use latest kernel
      kernelPackages = pkgs.linuxPackages_latest;

      # Only use swap for hibernate, because swap on SSD is bad
      kernel.sysctl = {
        "vm.swappiness" = 0;
      };

      # systemd-boot
      loader = {
        efi.canTouchEfiVariables = true;
        systemd-boot = {
          enable = true;
          configurationLimit = 5;
          editor = false;
        };
      };
    };

    # Add some extra drivers
    hardware.enableRedistributableFirmware = true;

    # Misc. settings
    documentation.dev.enable  = true;
    hardware.bluetooth.enable = true;
    # Can you system upgrade with nix flakes, though?
    # system.autoUpgrade = {
    #   enable = true;
    #   dates  = "16:00";
    # };
    time = {
      timeZone = "Europe/Stockholm";
      hardwareClockInLocalTime = true; # fuck windows
    };

    # TTY settings
    i18n.defaultLocale = "en_UK.UTF-8";
    console = {
      font   = "Lat2-Terminus16";
      keyMap = "dvorak";
    };

    # Networking
    networking.hostId = config.setup.networkId;
    networking.hostName = "samuel-${config.setup.name}";
    networking.networkmanager.enable = true;
    networking.firewall.enable = false;

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
