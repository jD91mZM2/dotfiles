{ config, pkgs, lib, ... }:
let
  inherit (lib) types;

  cfg = config.custom.services;
in
{
  options.custom.services = lib.mkOption {
    default = {};
    description = ''
      Custom scripts that should get their own dedicated user and
      service.
    '';
    example = {
      my-service.script = "sleep 1000";
    };
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        name = lib.mkOption {
          type = types.str;
          default = config._module.args.name;
          description = ''
            The name of the service. Defaults to the key.
          '';
        };
        script = lib.mkOption {
          type = types.str;
          example = "sleep 1000";
          description = ''
            The script that should run once this service is started.
          '';
        };
      };
    }));
  };
  config = {
    users.users = lib.listToAttrs (
      map
        (cfg: lib.nameValuePair cfg.name {
          createHome = true;
          home = "/var/lib/${cfg.name}";
        })
        (builtins.attrValues cfg)
    );
    systemd.services = lib.listToAttrs (
      map
        (cfg: lib.nameValuePair cfg.name {
          script = cfg.script;
          serviceConfig = {
            User = cfg.name;
            WorkingDirectory = "/var/lib/${cfg.name}";
            Restart = "always";
            RestartSec = "10s";
          };
          wantedBy = [ "multi-user.target" ];
        })
        (builtins.attrValues cfg)
    );
  };
}
