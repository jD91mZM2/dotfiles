{ pkgs, ... }:

{
  home = {
    # Set GTK theme
    gtk = {
      enable = true;
      theme = {
        name = "Dracula";
        package = pkgs.dracula-theme;
      };
    };
    # Use GTK theme for QT
    qt = {
      enable = true;
      platformTheme = "gtk";
    };

    # Cursor theme
    xsession.pointerCursor = {
      package = pkgs.xorg.xcursorthemes;
      name = "whiteglass";
      size = 16;
    };
  };
}
