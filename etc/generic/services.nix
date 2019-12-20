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
    trim.enable      = true;
  };
}
