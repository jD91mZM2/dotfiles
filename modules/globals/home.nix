{ pkgs, lib, config, inputs, ... }:

with lib;
{
  options = {
    homeUsers = mkOption {
      type = types.listOf types.str;
      description = "Activates home-manager for the given users";
      default = [ ];
    };

    # Used by configurations to add home-manager data
    home = mkOption {
      type = types.coercedTo types.unspecified toList (types.listOf types.unspecified);
      description = "List of home-manager modules";
      default = [ ];
    };
  };

  config = (
    let
      module = {
        imports = config.home;
      };
    in
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;

        users = builtins.listToAttrs (
          map
            (name: nameValuePair name module)
            config.homeUsers
        );
      };
    }
  );
}
