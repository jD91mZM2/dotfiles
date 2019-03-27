{ pkgs, ... }:
{
  systemd = {
    services.dropbox = {
      description = "Dropbox sync (rclone)";
      path = with pkgs; [ rclone ];
      script = ''
        #!/bin/sh
        set -e
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
      description = "Dropbox sync timer (rclone)";
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "1h";
        Unit = "dropbox.service";
      };
      wantedBy = [ "multi-user.target" ];
    };

    services.backup = {
      description = "Daily Backup";
      path = with pkgs; [ borgbackup coreutils rclone ];
      environment = {
        BORG_PASSPHRASE = (import ./secret.nix).backup_passphrase;
      };
      script = ''
        #!/bin/sh
        set -e
        cd ~/backup

        borg create ".::$(date)" ~/Coding/ ~/dotfiles ~/servers ~/Dropbox ~/Pictures \
          --progress \
          --stats \
          --compression lz4
        borg prune . --keep-daily 7
        rclone sync -v . "BackBlaze Backup:jD91mZM2-backups"
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
