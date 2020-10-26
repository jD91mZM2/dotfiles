{ config, pkgs, lib, ... }:

let
  cfg = config.setup.packages.graphical;
in
{
  options.setup.packages.graphical = with lib; {
    enable = mkEnableOption "Graphical applications";
  };

  config = {
    environment.systemPackages = with pkgs; lib.optionals cfg.enable [
      # Graphical - WM
      dmenu
      feh
      j4-dmenu-desktop
      networkmanagerapplet

      # Graphical
      firefox
      gnome3.zenity
      virtmanager
      xfce.xfce4-power-manager
    ];

    setup.home.modules = lib.singleton ({
      home.packages = with pkgs; lib.optionals cfg.enable [
        # Must have utils
        xclip
        xdotool

        # Applications
        abiword
        audacity
        bitwarden
        chromium
        filezilla
        firefox
        gimp
        inkscape
        keepassxc
        mpv
        mullvad-vpn
        zathura
        multimc
        musescore
        obs-studio
        olive-editor # <- THIS IS AMAZING
        pavucontrol
        scrcpy
        st
        # superTuxKart
        thunderbird
        vlc
        xorg.xev
        xorg.xwininfo

        # Desktop items
        (makeDesktopItem {
          name = "discord";
          desktopName = "Discord";
          exec = "${chromium}/bin/chromium --app=https://discordapp.com/channels/@me";
        })
        (makeDesktopItem {
          name = "redox-mattermost";
          desktopName = "Redox Mattermost";
          exec = "${chromium}/bin/chromium --app=https://chat.redox-os.org/";
        })
      ];
    });
  };
}
