{ pkgs, config, inputs, ... }:
{
  imports = [
    ./base.nix
    ./compositor.nix
    ./firefox.nix
    ./polybar.nix
    ./scripts.nix
    ./theme.nix
  ];

  services.xserver = {
    # Display Manager
    displayManager.gdm.enable = true;

    # AwesomeWM
    windowManager.awesome.enable = true;
  };

  # AwesomeWM config
  home.xdg.configFile."awesome".source = ./awesome-config;

  # Hack font for monospace
  fonts = {
    fonts = with pkgs; [ hack-font ];
    fontconfig.defaultFonts = {
      monospace = [ "Hack" ];
    };
  };
  home.xresources.properties."*.font" = "Hack:pixelsize=13:antialias=true:autohint=true";

  # Sound
  hardware.pulseaudio.enable = true;
  sound.enable = true;

  # Add background image
  environment.pathsToLink = [ "/share/backgrounds" ];
  environment.systemPackages = [
    pkgs.nixos-artwork.wallpapers.dracula
  ];
}
