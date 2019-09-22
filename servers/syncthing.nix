{ ... }:
{
  disabledModules = [ "services/networking/syncthing.nix" ];
  imports = [ <nixos-unstable/nixos/modules/services/networking/syncthing.nix> ];

  services.syncthing = {
    enable = true;
    declarative = {
      overrideDevices = true;
      devices = {
        computer = {
          id = "ILTIRMY-JT4SGSQ-AWETWCV-SLQYHE6-CY2YGAS-P3EGWY6-LSP7H4Z-F7ZQIAN";
          introducer = true;
        };
        phone = {
          id = "O7H6BPC-PKQPTT4-T4SEA7K-VI7HJ4K-J7ZJO5K-NWLNAK5-RBVCSBU-EXDHSA3";
        };
      };
      overrideFolders = false;
    };
  };
}
