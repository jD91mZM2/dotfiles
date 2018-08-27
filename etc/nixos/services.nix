{ pkgs, ... }:
{
  systemd = {
    services.dropbox = {
      description = "Mirrors dropbox using rclone";
      path = with pkgs; [ rclone ];
      script = ''
        #!/bin/sh
        cd ~/Dropbox

        # --update: Sync only if the timestamp is newer
        rclone copy --update . "Dropbox:"
        rclone copy --update "Dropbox:" .
      '';
      serviceConfig = {
        User = "user";
      };
    };
    timers.dropbox = {
      enable = true;
      description = "Automatically mirrors dropbox using rclone every hour";
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "1h";
        Unit = "dropbox.service";
      };
      wantedBy = [ "multi-user.target" ];
    };

    services.backup = {
      description = "Performs a backup of important files";
      path = with pkgs; [ borgbackup coreutils rclone ];
      environment = {
        BORG_PASSPHRASE = (import ./secret.nix).backup_passphrase;
      };
      script = ''
        #!/bin/sh

        borg create "$HOME/backup::$(date)" ~/Coding/ ~/dotfiles ~/Dropbox \
          --progress \
          --stats \
          --compression lz4
        rclone sync -v ~/backup "BackBlaze Backup:jD91mZM2-backups"
      '';
      serviceConfig = {
        User = "user";
      };
    };
    timers.backup = {
      enable = true;
      description = "Automatically perform a backup of important files every day";
      timerConfig = {
        OnCalendar = "*-*-* 16:00:00";
        Persistent = true;
        Unit = "backup.service";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
