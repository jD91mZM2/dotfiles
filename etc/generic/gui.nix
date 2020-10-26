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

  config = {
    setup.packages.graphical.enable = true;

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
          background = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg";
          greeters.gtk = {
            enable = true;
            theme = {
              name = "Yaru-dark";
              package = pkgs.yaru-theme;
            };
            iconTheme = {
              name = "Yaru";
              package = pkgs.yaru-theme;
            };
          };
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

    setup.home.modules = lib.singleton ({ config, ... }: {
      # Save awesome's config file
      home.file."Pictures/background.jpg".source = shared.background;
      xdg.dataFile."scripts".source = shared.scripts;
      xdg.configFile."awesome".source = config.lib.file.mkOutOfStoreSymlink ./awesome-config;

      # Configure xsession
      xsession = {
        enable = true;
        pointerCursor = {
          package = pkgs.xorg.xcursorthemes;
          name    = "whiteglass";
          size    = 16;
        };
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

        fade        = true;
        fadeDelta   = 5;
        inactiveDim = "0.1";
        shadow      = true;
      };

      # Xresources is kinda cool I guess :)
      xresources = {
        properties =
          (builtins.foldl'
            (x: y: x // y)
            {}
            (lib.zipListsWith
              (index: color: {
                "*.color${toString index.number}" = "#${color.rgb}";
              })
              shared.theme.colors
              shared.theme.xresources)
          ) // {
            # Everything
            "*.font" = "Hack:pixelsize=13:antialias=true:autohint=true";
            "*.background" = "#${(shared.theme.getColor 0).rgb}";
            "*.foreground" = "#${(shared.theme.getColor 5).rgb}";

            # XTerm stuff
            "XTerm.termName"          = "xterm-256color";
            "XTerm.vt100.faceName"    = "Hack:size =10";
            "XTerm*decTerminalID"     = "vt340";
            "XTerm*numColorRegisters" = 256;
          };
      };

      # GTK+ theme
      gtk = {
        enable = true;
        font = {
          name    = "Cantarell 11";
          package = pkgs.cantarell-fonts;
        };
        iconTheme = {
          name    = "Yaru";
          package = pkgs.yaru-theme;
        };
        theme = {
          name    = "Yaru-dark";
          package = pkgs.yaru-theme;
        };
      };

      # QT theme
      qt = {
        enable = true;
        platformTheme = "gtk";
      };
    });
  };
}
