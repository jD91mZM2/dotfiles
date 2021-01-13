{ pkgs, shared, ... }:
{
  # General things
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.upower.enable = true;

  # Flatpak
  xdg.portal.enable = true;
  services.flatpak.enable = true;

  # Nix lorri
  services.lorri.enable = true;

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
