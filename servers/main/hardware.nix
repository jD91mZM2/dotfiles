# Tweaked minimal config left by nixos-infect

{ modulesPath, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.loader.grub.device = "/dev/vda";

  fileSystems."/" = {
    device = "/dev/vda1";
    fsType = "ext4";
  };

  system.stateVersion = "21.03";
  home.home.stateVersion = "21.03";
}
