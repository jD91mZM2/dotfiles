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

    displayManager = {
      lightdm = {
        enable = true;
        background = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg";
        greeters.gtk = {
          enable = true;
          theme = {
            name = "Yaru-dark";
            package = pkgs.yaru-dracula-theme;
          };
          iconTheme = {
            name = "Yaru";
            package = pkgs.yaru-dracula-theme;
          };
        };
      };
    };
    windowManager.bspwm.enable = true;
    desktopManager.xterm.enable = false;
  };
}
