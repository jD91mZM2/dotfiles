{ pkgs, lib, config, ... }:

with lib;
let
  cfg = config.globals;
in
{
  imports = [
    ./home.nix
  ];

  options.globals = {
    keyboard = {
      layout = mkOption {
        type = types.str;
        description = "Which keyboard layout I'm using";
        default = "us";
      };
      variant = mkOption {
        type = types.str;
        description = "Which keyboard variant I'm using";
        default = "dvorak";
      };
    };
    colourscheme = mkOption {
      type = types.attrs;
      description = "Colours that look good";
      default = import ./colours.nix { inherit lib; };
    };
    userName = mkOption {
      type = types.str;
      description = "Username of my primary user";
      default = "user";
    };
  };
}
