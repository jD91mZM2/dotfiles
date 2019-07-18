{ pkgs, ... }:
{
  # Packages
  fonts.fonts = with pkgs; [
    cantarell-fonts
    comic-relief
    font-awesome-ttf
    hack-font
    symbola
  ];
  fonts.fontconfig.defaultFonts.monospace = [ "Hack" ];
}
