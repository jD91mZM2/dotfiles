{ pkgs, ... }:

let
  shared = import <dotfiles/shared> {};
in
{
  imports = [
    # Include results from hardware scan
    ./hardware-configuration.nix

    # Include setup that's generic across my devices
    ../generic
  ];

  setup = {
    name      = "computer";
    networkId = "c0122dbe";
  };

  # Make timeout long so I have time to plug in my keyboard. Using `null` here
  # seems to cause it to be selected immediately, unlike what the man page says
  boot.loader.timeout = 99;

  hardware.cpu.amd.updateMicrocode = true;

  services.xserver = {
    videoDrivers = [ "amdgpu" ];
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --primary
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --set TearFree on
    '';
  };

  # Syncthing
  services.syncthing = {
    enable = true;

    # Run as local user
    user = shared.consts.user;
    dataDir = "${shared.consts.home}/.local/share/Syncthing";

    declarative = {
      overrideDevices = true;
      devices = builtins.removeAttrs shared.consts.syncthingDevices [ "computer" ];
      overrideFolders = true;
      folders.main = {
        enable = true;
        path = "${shared.consts.home}/Sync";
        devices = [ "droplet" "rpi" "phone" "laptop" ];
      };
    };
  };

  # TODO: Backup
}
