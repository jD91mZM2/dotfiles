{ config, pkgs, ... }:
{
  ## https://github.com/NixOS/nixpkgs/issues/33231
  environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  # X server
  services.xserver = {
    enable = true;
    layout = "dvorak";
    xkbOptions = "compose:ralt";

    extraConfig = ''
      Section "Extensions"
        Option "DPMS" "Disable"
      EndSection
    '';

    videoDrivers = [ "intel" ];

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
}
