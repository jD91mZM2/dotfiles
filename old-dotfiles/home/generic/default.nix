{ pkgs, config, lib, ... }:

let
  cfg = config.setup;
in
{
  options.setup = with lib; {
    source = mkOption {
      type = types.path;
      description = ''
        Where to bind ~/.config/nixpkgs/home.nix
      '';
    };
  };

  imports = [
    ./env.nix
    ./fonts.nix
    ./misc.nix
    ./shells.nix
  ];

  config = {
    # g_get_user_special_dir(...) would return NULL, see
    # https://github.com/NixOS/nixpkgs/issues/95276
    xdg.userDirs.enable = true;

    # Don't mess with my keyboard layout!
    home.keyboard = null;
  };
}
