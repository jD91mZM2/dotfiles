{ ... }:
{
  disabledModules = [ "services/networking/syncthing.nix" ];
  imports = [ <nixos-unstable/nixos/modules/services/networking/syncthing.nix> ];

  services.syncthing = {
    enable = true;
    declarative = {
      overrideDevices = true;
      overrideFolders = false;
    };
  };
}
