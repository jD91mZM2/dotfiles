{ options, pkgs, lib, ... }:

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
    name      = "laptop";
    networkId = "e345d278";
  };

  hardware.cpu.intel.updateMicrocode = true;

  services.xserver = {
    # Configure video DPI and drivers
    dpi = 96;
    videoDrivers = [ "intel" ];

    # Automagically configure monitors
    displayManager.sessionCommands = ''
      if ${pkgs.xorg.xrandr}/bin/xrandr | grep "\<DP1\>.*\<connected\>"; then
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --pos 0x520 --primary --output DP1 --auto --pos 1366x0
      elif ${pkgs.xorg.xrandr}/bin/xrandr | grep "\<HDMI1\>.*\<connected\>"; then
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --primary --output HDMI1 --auto --right-of eDP1
      else
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --primary
      fi

      ${pkgs.xorg.xinput}/bin/xinput disable "$(${pkgs.xorg.xinput}/bin/xinput | awk -F= '/Touchpad/ { print int($2) }')"
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
      devices = builtins.removeAttrs shared.consts.syncthingDevices [ "laptop" ];
      overrideFolders = true;
      folders.main = {
        enable = true;
        path = "${shared.consts.home}/Sync";
        devices = [ "droplet" "rpi" "phone" "computer" ];
      };
    };
  };

  services.borgbackup.jobs.main = let
    repo = "${shared.consts.home}/backup";
  in {
    paths = map (s: "${shared.consts.home}/${s}") [ "dotfiles" "Coding" "Pictures" "Sync" ];
    inherit repo;
    encryption = {
      mode = "repokey";
      passCommand = "cat /root/borg-passphrase";
    };
    startAt = "16:00";
    prune.keep = {
      daily   = 7;
      weekly  = 4;
      monthly = 2;
    };
    postCreate = ''
      echo "\$archiveName = $archiveName"
      ${pkgs.rclone}/bin/rclone sync -v "${repo}" "BackBlaze:jD91mZM2-backups/primary"
      ${pkgs.rclone}/bin/rclone cleanup -v "BackBlaze:jD91mZM2-backups/primary"
    '';
  };
}
