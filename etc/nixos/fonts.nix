{ pkgs, ... }:
{
  # Packages
  fonts.fonts = with pkgs; [
    cantarell-fonts
    comic-relief
    font-awesome-ttf
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];
  fonts.fontconfig.defaultFonts.monospace = [ "Hack" ];
}
