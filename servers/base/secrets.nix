{ config, lib, ... }:
with lib;
let
  secrets = "${config.globals.syncthingHome}/secrets";
in
{
  options.deployment.secrets = mkOption {
    type = types.attrsOf (types.submodule ({ name, config, ... }: {
      options = {
        keySource = mkOption {
          type = types.str;
          description = "Relative path to key source file";
        };
      };
      config = {
        source = "${secrets}/${config.keySource}";
        destination = "/var/lib/secrets/${name}";
      };
    }));
  };
}
