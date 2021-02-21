{ pkgs, config, inputs, ... }:
{
  imports = [
    ./awesomewm.nix
    ./base.nix
    ./compositor.nix
    ./polybar.nix
    ./scripts.nix
    ./theme.nix
  ];

  # Display Manager
  services.xserver.displayManager.gdm.enable = true;

  # Sound
  hardware.pulseaudio.enable = true;
  sound.enable = true;

  # Add background image
  environment.pathsToLink = [ "/share/backgrounds" ];

  environment.systemPackages = with pkgs; [
    nixos-artwork.wallpapers.dracula

    # Fix XDG user directories
    xdg-user-dirs

    # Must have CLI utils
    xclip
    xdotool

    # Must have applications
    pavucontrol
  ];
}
