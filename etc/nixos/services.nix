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
      devices = {
        droplet = {
          id = "4JBUWER-ECEJGT7-XH6NFJB-F4WBHP2-CPREUK6-ETHPHHU-LXGPP3O-IAYLNAI";
          addresses = [ "tcp://krake.one:22000" ];
        };
        rpi.id = "AJEYZR5-OVJCWLD-SF37XSB-M2YSGMA-M7W33PW-S7JRWZM-ZLD6F33-KPSI3QD";
        phone.id = "O7H6BPC-PKQPTT4-T4SEA7K-VI7HJ4K-J7ZJO5K-NWLNAK5-RBVCSBU-EXDHSA3";
      };
      overrideFolders = true;
      folders.main = {
        enable = true;
        path = "/${shared.consts.home}/Sync";
        devices = [ "droplet" "rpi" "phone" ];
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
      hourly = 5;
      daily = 3;
      weekly = 2;
      monthly = 0;
    };
    autoScrub.enable = true;
  };
  services.borgbackup.jobs.main = let
    repo = "${shared.consts.home}/backup";
  in {
    paths = map (s: "${shared.consts.home}/${s}") [ "Coding" "dotfiles" "servers" "Nextcloud" "Pictures" ];
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

        ${pkgs.rclone}/bin/rclone sync -v "${shared.consts.home}/Nextcloud" "Dropbox:"
      '';
    };
  };
}
