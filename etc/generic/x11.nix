{ pkgs, lib, ... }:
{
  ## https://github.com/NixOS/nixpkgs/issues/33231
  environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  # Enable 32-bit OpenGL for steam
  hardware.opengl = {
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
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
    enableCtrlAltBackspace = true;

    # Enable driver for various input devices - While I think it's
    # mainly for touchpads, without it, my keyboard layout doesn't
    # seem to be applied.
    libinput.enable = true;

    layout = "us,us";
    xkbVariant = "dvorak,";
    xkbOptions = "compose:ralt,terminate:ctrl_alt_bksp,grp:ctrl_shift_toggle";

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
      session = lib.singleton {
        name = "AwesomeWM";
        manage = "window";

        # There's no start script here, because home-manager manages my session.
        start = ''
          sleep infinity &
          waitPID="$!"
        '';
      };
    };
    desktopManager.xterm.enable = false;
  };
}
