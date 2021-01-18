{ pkgs, ... }:
let
  inherit (pkgs) surf;
in
{
  environment.systemPackages = [
    surf

    (pkgs.makeDesktopItem {
      name = "discord";
      desktopName = "Discord";
      exec = "${surf}/bin/surf \"https://discord.com/channels/@me/\"";
    })
    (pkgs.makeDesktopItem {
      name = "redox-mattermost";
      desktopName = "Redox Mattermost";
      exec = "${surf}/bin/surf \"https://chat.redox-os.org/\"";
    })
  ];

  home.xsession.initExtra = ''
    ${surf}/bin/surf "https://discord.com/channels/@me/"
    ${surf}/bin/surf "https://chat.redox-os.org/"
  '';
}
