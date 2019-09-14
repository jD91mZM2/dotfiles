{ pkgs, ... }:
let
  home = "/home/user";
in
{
  # General things
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.locate = {
    enable = true;
    interval = "07:00 PM";
  };
  services.upower.enable = true;

  # Backup
  services.zfs = {
    autoSnapshot = {
      enable = true;

      # I never use these anyway, as everything I throw away goes in a
      # trashcan.
      frequent = 2;
      hourly = 5;
      daily = 3;
      weekly = 2;
      monthly = 0;
    };
    autoScrub.enable = true;
  };
  services.borgbackup.jobs.main = let
    repo = "${home}/backup";
  in {
    paths = map (s: "${home}/${s}") [ "Coding" "dotfiles" "servers" "Nextcloud" "Pictures" ];
    inherit repo;
    doInit = true;
    encryption = {
      mode = "repokey";
      passCommand = "cat /root/borg-passphrase";
    };
    startAt = "04:00 PM";
    prune.keep = {
      daily = 7;
      weekly = 4;
      monthly = 2;
    };
    postCreate = ''
      echo "\$archiveName = $archiveName"
      ${pkgs.rclone}/bin/rclone sync -v "${repo}" BackBlaze:jD91mZM2-backups
      ${pkgs.rclone}/bin/rclone cleanup -v BackBlaze:jD91mZM2-backups
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

        ${pkgs.rclone}/bin/rclone sync -v "${home}/Nextcloud" "Dropbox:"
      '';
    };
  };
}
