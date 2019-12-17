{ pkgs, ... }:
let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  # General things
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.locate = {
    enable = true;
    interval = "07:00 PM";
  };
  services.upower.enable = true;

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
        devices = [ "droplet" "rpi" "phone" ];
      };
      # For when I don't feel comfortable putting my whole password
      # vault on a device...
      folders.untrustworthy = {
        enable = true;
        path = "/${shared.consts.home}/Sync/untrustworthy";
        devices = [ "droplet" "rpi" "phone" "school" ];
      };
    };
  };

  # Backup
  services.zfs = {
    autoSnapshot = {
      enable = true;

      # I never use these anyway, as everything I throw away goes in a
      # trashcan.
      frequent = 2;
      hourly   = 5;
      daily    = 3;
      weekly   = 2;
      monthly  = 0;
    };
    autoScrub.enable = true;
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

  # Custom services
  systemd = {
    services.dropbox = {
      description = "Backup Nextcloud to Dropbox";
      startAt = "hourly";
      script = ''
        #!/bin/sh
        set -e

        ${pkgs.rclone}/bin/rclone sync -v "${shared.consts.home}/Nextcloud" "Dropbox:"
      '';
    };
  };
}
