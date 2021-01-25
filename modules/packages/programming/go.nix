{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Go itself
    go

    # Editor tooling
    gotools
    go-langserver
  ];

  home = { config, ... }: {
    home.sessionVariables = {
      GOPATH = "${config.xdg.dataHome}/go";
      GOCACHE = "${config.xdg.cacheHome}/go";
    };
  };
}
