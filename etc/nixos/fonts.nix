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

  # TTY settings
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
}
