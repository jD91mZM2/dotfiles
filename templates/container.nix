{ pkgs, config, lib, ... }:

{
  imports = [
    # Base modules
    ../modules/base
    ../modules/meta.nix
    ../modules/user

    # CLI Packages
    ../modules/packages/git.nix
    ../modules/packages/inc.nix
    ../modules/packages/neovim.nix
  ];

  users = {
    mutableUsers = false;
    users."${config.globals.userName}" = {
      # This is a hardcoded uid, used by any container. This means files shared
      # with containers are accessible.
      uid = 1000;

      isNormalUser = true;

      # No password
      hashedPassword = "";
    };
  };

  environment.variables.TERM = "st-256color";
}
