{ pkgs, ... }:
{
  # Packages
  fonts = {
    fonts = with pkgs; [
      cantarell-fonts
      comic-relief
      font-awesome
      hack-font
      noto-fonts
    ];
    fontconfig.defaultFonts.monospace = [ "Hack" ];
  };
}
