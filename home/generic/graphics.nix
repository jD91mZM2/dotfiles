{ pkgs, config, lib, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  cfg = config.setup.graphics;
in {
  options.setup.graphics = {
    enable = lib.mkEnableOption "graphics";
  };

  imports = [
    ./polybar.nix
  ];

  config = lib.mkIf cfg.enable {
    # Enable polybar
    setup.graphics.polybar.enable = true;

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
      windowManager.awesome = {
        enable = true;
      };
    };

    # Composite manager
    services.picom = {
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
        package = pkgs.yaru-dracula-theme;
      };
      theme = {
        name    = "Yaru-dark";
        package = pkgs.yaru-dracula-theme;
      };
    };

    # QT theme
    qt = {
      enable = true;
      platformTheme = "gtk";
    };
  };
}
