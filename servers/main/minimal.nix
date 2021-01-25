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

  boot.cleanTmpDir = true;

  networking.hostName = "main";
  networking.firewall.allowPing = true;

  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRNU1yPnVxZtK/qrOkAnp5J+EqXJ6wTeXOScw2lhqWg (none)"
  ];
}
