{ pkgs, ... }:
{
  # Packages
  fonts = {
    fonts = with pkgs; [
      hack-font
    ];
    fontconfig.defaultFonts.monospace = [ "Hack" ];
  };
}
