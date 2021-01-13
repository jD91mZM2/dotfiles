{ config, pkgs, lib, shared, ... }:

let
  cfg = config.setup.graphics;
in
{
  options.setup.graphics = with lib; {
    enable = mkEnableOption "Graphics & Sound";

    fast = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to disable "pointless" stuff like a composite manager.
      '';
    };
  };

  imports = [
    ./style.nix
    ./polybar.nix
    ./firefox.nix
  ];

  config = lib.mkIf cfg.enable {
    setup.packages.graphics.enable = true;
    setup.graphics.polybar.enable = true;
    setup.graphics.firefox.enable = true;

    ## https://github.com/NixOS/nixpkgs/issues/33231
    environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

    # Fonts
    fonts = {
      fonts = with pkgs; [
        hack-font
        emacs-all-the-icons-fonts
      ];
      fontconfig.defaultFonts.monospace = [ "Hack" ];
    };

    # Sound
    hardware.pulseaudio.enable = true;
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
      xkbOptions = "compose:ralt,terminate:ctrl_alt_bksp,grp:alt_shift_toggle";

      # LightDM display manager
      displayManager = {
        lightdm = {
          enable = true;
          greeters.gtk.enable = true;
        };

        # Dummy session
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

    setup.home.modules = lib.singleton {
      # Save awesome's config file
      xdg.dataFile."scripts".source = shared.scripts;
      xdg.configFile."awesome".source = ./awesome-config;

      # Configure xsession
      xsession = {
        enable = true;
        initExtra = ''
          ${pkgs.stdenv.shell} "${shared.scripts}/startup.sh" &
        '';

        # AwesomeWM
        windowManager.awesome.enable = true;
      };

      # Composite manager
      services.picom = lib.mkIf (!cfg.fast) {
        enable = true;
        # Apparently, using (intel + xrandr to configure multiple monitors + glx
        # backend) seems to cause all kinds of weird issues.
        backend = "xrender";

        fade = true;
        fadeDelta = 5;
        inactiveDim = "0.1";
        shadow = true;
      };
    };
  };
}
