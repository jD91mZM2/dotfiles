{ config, ... }:
let
  home = "/home/${config.globals.userName}";
in
{
  services.syncthing = {
    enable = true;

    user = config.globals.userName;
    dataDir = "${home}/.local/share/Syncthing";

    declarative = {
      # Set folders
      overrideFolders = true;
      folders.main = {
        path = "${home}/Sync";
        devices = [ "laptop" "computer" "droplet" "phone" ];
      };

      # List of all devices
      overrideDevices = true;
      devices = {
        laptop = {
          id = "ILTIRMY-JT4SGSQ-AWETWCV-SLQYHE6-CY2YGAS-P3EGWY6-LSP7H4Z-F7ZQIAN";
          introducer = true;
        };
        computer = {
          id = "2U6NZVJ-PNN6BW5-4FXVOUH-DLOIQRT-IPZIDNB-FHQTKPL-OQGYFOA-QBZ3SQU";
          introducer = true;
        };
        droplet = {
          id = "4JBUWER-ECEJGT7-XH6NFJB-F4WBHP2-CPREUK6-ETHPHHU-LXGPP3O-IAYLNAI";
          addresses = [ "tcp://krake.one:22000" ];
        };
        phone.id = "O7H6BPC-PKQPTT4-T4SEA7K-VI7HJ4K-J7ZJO5K-NWLNAK5-RBVCSBU-EXDHSA3";
        school.id = "6YYJM7K-ZP3CXHB-P4KU6CF-PVF4RG4-MFGBGXG-CUGZ26X-Z42TS6Q-BVHWKQP";
      };
    };
  };
}
