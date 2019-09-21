{ ... }:
{
  imports = [
    # Base on top of the regular KDE USB
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-graphical-kde.nix>
    # Include a nixpkgs channel
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  boot.supportedFilesystems = [ "zfs" ];

  i18n.consoleKeyMap = "dvorak";
  services.xserver.layout = "dvorak";
}
