{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Backup DVDs
    dvdbackup

    # Upload to cloud
    rclone
  ];
}
