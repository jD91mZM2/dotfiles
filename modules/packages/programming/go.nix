{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Go itself
    go

    # Editor tooling
    gotools
    go-langserver
  ];
}
