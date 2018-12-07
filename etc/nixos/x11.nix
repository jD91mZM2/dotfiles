{ config, pkgs, ... }:
{
  ## Required by xfce4-panel
  environment.pathsToLink = [ "/share/xfce4" ];
  ## https://github.com/NixOS/nixpkgs/issues/33231
  environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  # X server
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "compose:ralt";

    videoDrivers = [ "nvidia" ];
    dpi = 96; # nvidia detects a very, very small size by default

    # Touchpad:
    # libinput.enable = true;

    displayManager.lightdm = {
      enable = true;
      background = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg";
      greeters.gtk = {
        enable = true;
        theme = {
          name = "Adapta";
          package = pkgs.adapta-gtk-theme;
        };
      };
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  # Graphical programs
  programs.slock.enable = true;
  programs.dconf.enable = true;
}
