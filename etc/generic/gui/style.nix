{ config, lib, pkgs, shared, ... }:

lib.mkIf config.setup.graphics.enable {
  services.xserver.displayManager.lightdm = {
    background = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg";
    greeters.gtk = {
      theme = shared.theme.gtkTheme;
      iconTheme = shared.theme.iconTheme;
    };
  };

  setup.home.modules = lib.singleton ({ config, ... }: {
    home.file."Pictures/background.jpg".source = shared.background;

    xsession.pointerCursor = {
      package = pkgs.xorg.xcursorthemes;
      name    = "whiteglass";
      size    = 16;
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
      iconTheme = shared.theme.iconTheme;
      theme = shared.theme.gtkTheme;
    };

    # QT theme
    qt = {
      enable = true;
      platformTheme = "gtk";
    };
  });
}
