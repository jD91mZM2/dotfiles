{ pkgs, ... }:
{
  home.file = {
    "Pictures/Backgrounds/background.jpg".source = pkgs.background;
    "Pictures/Backgrounds/background-focus.jpg".source = pkgs.background-focus;
  };
  xdg.configLink = {
    "bspwm".source = ./bspwm-config/bspwm;
    "sxhkd".source = ./bspwm-config/sxhkd;
  };
  xsession = {
    enable = true;
    pointerCursor = {
      package = pkgs.xorg.xcursorthemes;
      name    = "whiteglass";
      size    = 16;
    };
    windowManager.command = ''
      ${pkgs.sxhkd}/bin/sxhkd &
      ${pkgs.bspwm}/bin/bspwm
    '';
  };
}
