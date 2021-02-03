{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Go itself
    go

    # Editor tooling
    gopls
    gotools
  ];

  home = { config, ... }: {
    home.sessionVariables = {
      GOPATH = "${config.xdg.dataHome}/go";
      GOCACHE = "${config.xdg.cacheHome}/go";
    };
  };
}
