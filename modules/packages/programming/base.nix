{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    pre-commit
  ];
}
