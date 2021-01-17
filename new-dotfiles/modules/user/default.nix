{ config, ... }:
{
  imports = [
    ./git.nix
  ];

  users.users."${config.globals.userName}" = {
    # This is a hardcoded uid, used by any container. This means files shared
    # with containers are accessible.
    uid = 1000;

    isNormalUser = true;
    extraGroups = [ "wheel" ];
    initialPassword = "nixos";
  };

  homeUsers = [ config.globals.userName ];
}
