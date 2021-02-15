{ pkgs, self, system, ... }:
{
  # Install podman for containers
  # virtualisation.podman.enable = true;

  # Install qemu VM
  environment.systemPackages = with pkgs; [
    qemu
  ];
}
