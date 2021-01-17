{ config, ... }:
{
  home.programs.git = {
    enable = true;
    userName = config.globals.name;
    userEmail = config.globals.email;
  };
}
