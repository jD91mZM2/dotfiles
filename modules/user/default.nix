{ config, ... }:
{
  imports = [
    ./git.nix
    ./ssh.nix
  ];

  users.users."${config.globals.userName}" = {
    # This is a hardcoded uid, used by any container. This means files shared
    # with containers are accessible.
    uid = 1000;

    isNormalUser = true;
    extraGroups = [
      # Sudo user
      "wheel"

      # Allow networkmanager
      "networkmanager"

      # Allow adb control
      "adbusers"
    ];
  };

  homeUsers = [ config.globals.userName ];
}
