{ modulesPath, lib, ... }:

{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-base.nix"

    ../../modules/base
    ../../modules/console.nix
    ../../modules/meta.nix
    ../../modules/x11

    ../../modules/packages/neovim.nix
  ];

  isoImage.isoName = lib.mkForce "nixos.iso";

  homeUsers = [ "nixos" ];

  system.stateVersion = "21.03";
  home.home.stateVersion = "21.03";
}
