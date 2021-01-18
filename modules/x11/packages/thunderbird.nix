{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.thunderbird
  ];

  home.xsession.initExtra = ''
    ${pkgs.thunderbird}/bin/thunderbird &
  '';
}
