{ pkgs, ... }:
{
  ## https://github.com/NixOS/nixpkgs/issues/33231
  environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  hardware.cpu.intel.updateMicrocode = true;
  hardware.opengl.driSupport32Bit = true;

  # Sound
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };
  sound.enable = true;

  # X server
  services.xserver = {
    enable = true;
    enableCtrlAltBackspace = true;
    layout = "dvorak";
    xkbOptions = "compose:ralt";

    videoDrivers = [ "intel" ];
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
            name = "Yaru-dark";
            package = pkgs.yaru-theme-latest;
          };
          iconTheme = {
            name = "Yaru";
            package = pkgs.yaru-theme-latest;
          };
        };
      };
      sessionCommands = ''
        if ${pkgs.xorg.xrandr}/bin/xrandr | grep "\<DP1\>.*\<connected\>"; then
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --pos 0x520 --primary --output DP1 --auto --pos 1366x0
        elif ${pkgs.xorg.xrandr}/bin/xrandr | grep "\<HDMI1\>.*\<connected\>"; then
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --primary --output HDMI1 --auto --right-of eDP1
        else
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --primary
        fi

        ${pkgs.xorg.xinput}/bin/xinput disable "$(${pkgs.xorg.xinput}/bin/xinput | awk -F= '/Touchpad/ { print int($2) }')"
      '';
    };
    windowManager.bspwm.enable = true;
    desktopManager.xterm.enable = false;
  };
}
