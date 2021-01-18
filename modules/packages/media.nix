{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    sxiv
    vlc
  ];
}
