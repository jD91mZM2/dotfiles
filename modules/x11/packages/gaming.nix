{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    multimc
  ];
}
