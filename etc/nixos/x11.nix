{ config, pkgs, ... }:
{
  ## https://github.com/NixOS/nixpkgs/issues/33231
  environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  hardware.cpu.intel.updateMicrocode = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.nvidia = {
    modesetting.enable = true;
    optimus_prime = {
      enable = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:4:0:0";
    };
  };

  # Sound
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };
  sound.enable = true;

  # X server
  services.xserver = {
    enable = true;
    layout = "dvorak";
    xkbOptions = "compose:ralt";

    videoDrivers = [ "nvidia" ];
    dpi = 96;

    # Touchpad:
    # libinput.enable = true;

    displayManager = {
      lightdm = {
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
      sessionCommands = ''
        if ${pkgs.xorg.xrandr}/bin/xrandr | grep "\<DP-1-1\>.*\<connected\>"; then
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1-1 --pos 0x520 --primary --output DP-1-1 --pos 1366x0
        elif ${pkgs.xorg.xrandr}/bin/xrandr | grep "\<HDMI-1-1\>.*\<connected\>"; then
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1-1 --primary --output HDMI-1-1 --right-of eDP-1-1
        else
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1-1 --primary
        fi
      '';
    };
    windowManager.bspwm.enable = true;
  };
}
