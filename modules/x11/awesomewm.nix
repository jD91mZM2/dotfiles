{ pkgs, ... }:
{
  # System-wide installation of AwesomeWM
  services.xserver.windowManager.awesome.enable = true;

  home = {
    # Install my AwesomeWM config
    xdg.configFile."awesome".source = ./awesome-config;

    # If the user is using home-manager, they will want AwesomeWM configured
    # using it in order to properly load all environment variables and start
    # all user services.
    xsession = {
      enable = true;
      windowManager.awesome.enable = true;

      initExtra = ''
        "${pkgs.dex}/bin/dex" -a
      '';
    };
  };
}
