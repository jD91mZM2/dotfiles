{ pkgs, lib, config, ... }:

with lib;
{
  options.desktopItems = mkOption {
    type = types.attrsOf (types.submodule {
      options = {
        name = mkOption {
          type = types.str;
          description = "Desktop Entry Name";
        };
        exec = mkOption {
          type = types.str;
          description = "Command to run";
        };
        autostart = mkOption {
          type = types.bool;
          description = "Whether to autostart this desktop item";
          default = false;
        };
      };
    });
    description = "Desktop Items to install";
    default = { };
  };

  config = {
    environment.systemPackages = concatLists (flip mapAttrsToList config.desktopItems (name: desktop:
      let
        desktopPackage = pkgs.makeDesktopItem {
          inherit name;
          desktopName = desktop.name;
          inherit (desktop) exec;
        };
      in
      singleton desktopPackage ++ optional desktop.autostart (pkgs.makeAutostartItem {
        inherit name;
        package = desktopPackage;
      })
    ));
  };
}
