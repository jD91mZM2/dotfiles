{ pkgs, ... }:
{
  environment.variables = {
    # TODO: Put this in home-manager, once that environment actually affects X11
    QT_QPA_PLATFORMTHEME = "gtk2";
  };
}
