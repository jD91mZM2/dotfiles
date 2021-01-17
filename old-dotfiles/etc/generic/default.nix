# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, config, lib, pkgs, shared, inputs, system, ... }:

let
  cfg = config.setup;
in
{
  options.setup = with lib; {
    name = mkOption {
      type = types.nullOr types.str;
      description = "The name of this deployment";
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
    ./gui
    ./hardware.nix
    ./home.nix
    ./meta.nix
    ./packages
    ./services.nix
  ];

  config = {
    setup = lib.mkIf cfg.full {
      boot = true;
      network = true;

      graphics.enable = true;

      packages = {
        gpg.enable = true;
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

    # System overlays
    nixpkgs.overlays = [ self.overlay ];

    # Specify hostname, if set
    networking = lib.mkIf (cfg.name != null) {
      hostName = "samuel-${cfg.name}";
    };

    # Enable SYSRQ keys because disabling that is a horrible idea I think
    boot.kernel.sysctl."kernel.sysrq" = 1;

    # Misc. settings
    documentation.dev.enable = true;
    hardware.bluetooth.enable = true;
    time.hardwareClockInLocalTime = true; # fuck windows

    # Mime type for wasm, see https://github.com/mdn/webassembly-examples/issues/5
    environment.etc."mime.types".text = ''
      application/wasm  wasm
    '';

    # Language settings
    time.timeZone = "Europe/Stockholm";
    i18n.defaultLocale = "en_GB.UTF-8";
    console = {
      keyMap = "dvorak";
      packages = [ pkgs.powerline-fonts ];
      font = "ter-powerline-v16n";
    };

    # User settings
    users = {
      # Use z shell
      defaultUserShell = inputs.nix-exprs.packages."${system}".zsh;

      # Create standard user
      users."${shared.consts.user}" = {
        # This is a hardcoded uid, used by any container. This means files shared
        # with containers are accessible.
        uid = 1000;

        isNormalUser = true;
        extraGroups = [ "libvirtd" "adbusers" "wheel" ];
      };
    };

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "18.03"; # Did you read the comment?
  };
}
