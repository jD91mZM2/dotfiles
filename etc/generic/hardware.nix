{ config, lib, pkgs, ... }:

let
  cfg = config.setup;
in {
  options.setup = with lib; {
    boot = mkOption {
      type = types.bool;
      description = "Whether to enable the bootloader";
      default = false;
    };
    network = mkOption {
      type = types.bool;
      description = "Whether to enable networking stuff";
      default = false;
    };
  };

  config = {
    boot = lib.mkIf cfg.boot {
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

    networking = lib.mkIf cfg.network {
      networkmanager.enable = true;
      firewall.enable = false;
    };
  };
}
