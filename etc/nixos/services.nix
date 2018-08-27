{ pkgs, ... }:
{
  systemd = {
    services.dropbox = {
      description = "Mirrors dropbox using rclone";
      script = ''
        #!/bin/sh
        cd $HOME/Dropbox

        # --update: Sync only if the timestamp is newer
        ${pkgs.rclone}/bin/rclone copy --update . "Dropbox:"
        ${pkgs.rclone}/bin/rclone copy --update "Dropbox:" .
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
  };
}
