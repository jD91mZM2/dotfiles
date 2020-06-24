{ config, lib, pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
      cantarell-fonts
      comic-relief
      font-awesome
      noto-fonts
  ];
}
