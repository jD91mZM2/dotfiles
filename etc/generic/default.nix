# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, shared, ... }:

let
  cfg = config.setup;
in {
  options.setup = with lib; {
    name = mkOption {
      type = types.str;
      description = "The name of this deployment, same as the folder's name in /etc.";
    };
    networkId = mkOption {
      type = types.nullOr types.str;
      description = "Same as network.hostId, obtain using `head -c8 /etc/machine-id`";
      default = null;
    };

    full = mkOption {
      type = types.bool;
      default = false;
      description = "Installs all the bells and whistles. Just an alias for enabling different components.";
    };
  };

  imports = [
    # Files
    ./containers.nix
    ./gui.nix
    ./home.nix
    ./meta.nix
    ./packages
    ./services.nix
    ./sudo.nix
  ];

  config = {
    setup = lib.mkIf cfg.full {
      graphics.enable = true;

      packages = {
        graphical.enable = true;

        languages = {
          c = true;
          elm = true;
          go = true;
          haskell = true;
          java = true;
          latex = true;
          markdown = true;
          python = true;
          rust = true;
          wasm = true;
        };
      };
    };

    boot = {
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
