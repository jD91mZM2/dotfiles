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

  hardware.cpu.amd.updateMicrocode = true;

  services.xserver = {
    videoDrivers = [ "amdgpu" ];
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-2 --primary
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
      devices = shared.utils.without [ "computer" ] shared.consts.syncthingDevices;
      overrideFolders = true;
      folders.main = {
        enable = true;
        path = "/${shared.consts.home}/Sync/main";
        devices = [ "droplet" "rpi" "phone" "laptop" ];
      };
    };
  };

  # TODO: Backup
}
