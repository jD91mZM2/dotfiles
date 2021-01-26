{ config, ... }:
{
  imports = [
    ../../modules/user/extra/syncthing.nix
  ];

  deployment.secrets =
    let
      secrets = "${config.globals.syncthingHome}/secrets";
    in
    {
      syncthing-key = {
        keySource = "syncthing/krake.one/key.pem";
        owner.user = config.globals.userName;
      };
      syncthing-cert = {
        keySource = "syncthing/krake.one/cert.pem";
        owner.user = config.globals.userName;
      };
    };

  # Syncthing
  services.syncthing.declarative = {
    key = "/run/keys/syncthing-key";
    cert = "/run/keys/syncthing-cert";
  };
}
