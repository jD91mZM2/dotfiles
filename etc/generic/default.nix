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
    ./containers.nix
    ./fonts.nix
    ./gui.nix
    ./packages.nix
    ./services.nix
    ./sudo.nix
  ];

  config = {
    nix = {
      # Nix flakes
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';

      binaryCaches = [
        # Input Output HK - Hydra cache for haskell.nix
        "https://hydra.iohk.io"

        # Cachix Caches
        "https://iohk.cachix.org"
        "https://nix-community.cachix.org"
        "https://jd91mzm2.cachix.org"
      ];
      binaryCachePublicKeys = [
        # Input Output HK - Hydra cache for haskell.nix
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="

        # Cachix Caches
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "jd91mzm2.cachix.org-1:Ozz1Jhba7XCQidfZuptUFWy0tpnqtg6v1gJpuLIkyyY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];

      registry.nixpkgs = {
        from = {
          type = "indirect";
          id = "nixpkgs";
        };
        to = lib.mkForce {
          type  = "github";
          owner = "NixOS";
          repo  = "nixpkgs";
          rev   = "nixos-unstable";
        };
      };

      # Add dotfiles to my nix path - for now. This should be removed once I go full flake
      nixPath = [
        "dotfiles=${shared.consts.dotfiles}"
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
    time.hardwareClockInLocalTime = true; # fuck windows

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
      initialPassword = "nixos";
      isNormalUser    = true;
      extraGroups     = [ "libvirtd" "adbusers" ];
    };

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "18.03"; # Did you read the comment?
  };
}
