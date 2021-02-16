{ pkgs, lib, config, ... }:

with lib;
let
  cfg = config.globals;
in
{
  imports = [
    ./home.nix
    ./desktop.nix
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
    name = mkOption {
      type = types.str;
      description = "My online alias";
      default = "jD91mZM2";
    };
    email = mkOption {
      type = types.str;
      description = "My email address";
      default = "me@krake.one";
    };

    sshKey = mkOption {
      type = types.str;
      description = "My SSH public key - managed by GPG";
      default = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRNU1yPnVxZtK/qrOkAnp5J+EqXJ6wTeXOScw2lhqWg (none)";
    };
  };
}
