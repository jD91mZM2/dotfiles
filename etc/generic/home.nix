{ config, lib, pkgs, system, self, shared, inputs, ... }:

let
  cfg = config.setup.home;
in
{
  options.setup.home = with lib; {
    users = mkOption {
      type = types.listOf types.str;
      default = singleton shared.consts.user;
      description = "Users to enable home-manager for";
    };
    modules = mkOption {
      type = types.listOf types.unspecified;
      description = "Home-manager configurations and customisations";
    };
  };

  config = {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;

      users = lib.genAttrs cfg.users (_user: {
        # Hacky way to send extraArgs to home-manager
        _module.args = self.lib.system."${system}".configInputs.extraArgs;

        imports = [
          inputs.nur.hmModules.programs
          ../../home/full.nix
        ] ++ cfg.modules;
      });
    };
  };
}
