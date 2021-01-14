{ shared, lib, modulesPath, ... }:

{
  imports = [
    # Include setup that's generic across my devices
    ../generic

    # Include installation base and hardware support
    "${modulesPath}/installer/cd-dvd/iso-image.nix"
    "${modulesPath}/profiles/all-hardware.nix"
  ];

  isoImage = {
    isoName = "nixos.iso";
    makeEfiBootable = true;
    makeUsbBootable = true;
  };

  setup = {
    graphics.enable = true;
    network = true;
  };

  # No user password
  users.users."${shared.consts.user}".hashedPassword = "";

  # User does not need password
  security.sudo.wheelNeedsPassword = false;
}
