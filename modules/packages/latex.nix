{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Install texlive
    texlive.combined.scheme-medium

    # Language Server
    texlab

    # Install PDF viewer
    zathura
  ];
}
