{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Backup DVDs
    dvdbackup
    handbrake

    # Upload to cloud
    rclone
  ];
}
