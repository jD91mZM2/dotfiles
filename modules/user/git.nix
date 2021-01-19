{ config, ... }:
{
  home.programs.git = {
    userName = config.globals.name;
    userEmail = config.globals.email;
  };
}
