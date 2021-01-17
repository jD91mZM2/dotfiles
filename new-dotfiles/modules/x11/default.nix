{ pkgs, config, inputs, ... }:
{
  imports = [
    ./base.nix
    ./firefox.nix
    ./polybar.nix
    ./scripts.nix
  ];

  services.xserver = {
    # Display Manager
    displayManager.gdm.enable = true;

    # AwesomeWM
    windowManager.awesome.enable = true;
  };

  # AwesomeWM config
  home.xdg.configFile."awesome".source = ./awesome-config;

  # Hack font
  fonts = {
    fonts = with pkgs; [ hack-font ];
    fontconfig.defaultFonts = {
      monospace = [ "Hack" ];
    };
  };
  home.xresources.properties."*.font" = "Hack:pixelsize=13:antialias=true:autohint=true";

  # Add background image
  environment.pathsToLink = [ "/share/backgrounds" ];
  environment.systemPackages = [
    pkgs.nixos-artwork.wallpapers.dracula
  ];
}
