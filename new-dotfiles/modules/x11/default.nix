{ pkgs, config, inputs, ... }:
{
  imports = [
    ./base.nix
    ./scripts.nix
  ];

  services.xserver = {
    # Display Manager
    displayManager.gdm.enable = true;

    # AwesomeWM
    windowManager.awesome.enable = true;
  };

  # Add background image
  environment.pathsToLink = [ "/share/backgrounds" ];
  environment.systemPackages = [
    pkgs.nixos-artwork.wallpapers.dracula
  ];

  home = {
    xdg.configFile."awesome".source = ./awesome-config;
  };
}
