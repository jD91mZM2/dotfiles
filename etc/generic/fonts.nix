{ pkgs, ... }:
{
  # Packages
  fonts = {
    fonts = with pkgs; [
      cantarell-fonts
      comic-relief
      font-awesome
      hack-font
      symbola
    ];
    fontconfig.defaultFonts.monospace = [ "Hack" ];
  };
}
