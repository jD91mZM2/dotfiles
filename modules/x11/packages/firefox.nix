{ pkgs, lib, self, system, config, ... }:

with lib;

let
  inherit (self.packages."${system}") firefox;

  home = "/home/${config.globals.userName}";
  mkApp = url: ''"${firefox}/bin/firefox" --profile ${home}/.mozilla/firefox/app --new-window "${url}"'';

  discord = mkApp "https://discord.com/channels/@me/";
  mattermost = mkApp "https://chat.redox-os.org/";

  preferences = {
    # Allow userChrome.css
    "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

    # Preferences as suggested by https://www.privacytools.io/browsers/#about_config
    "privacy.firstparty.isolate" = true;
    "privacy.resistFingerprinting" = true;
    "privacy.trackingprotection.fingerprinting.enabled" = true;
    "privacy.trackingprotection.cryptomining.enabled" = true;
    "privacy.trackingprotection.enabled" = true;

    "beacon.enabled" = false;
  };
in
{
  environment.systemPackages = [
    firefox
  ];

  desktopItems = {
    discord = {
      name = "Discord";
      exec = mkApp "https://discord.com/channels/@me/";
      autostart = true;
    };
    redox-mattermost = {
      name = "Redox Mattermost";
      exec = mkApp "https://chat.redox-os.org/";
      autostart = true;
    };
  };

  home.programs.firefox = {
    enable = true;
    package = self.packages."${system}".firefox;

    profiles = {
      default = {
        id = 0;
        name = "Default Profile";
        settings = preferences;
      };
      app = {
        id = 1;
        name = "Application View";
        settings = preferences;

        userChrome = ''
          @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");

          #navigator-toolbox {
            /* display: none; does not work for some reason */
            overflow: hidden;
            height: 0;
          }
        '';
      };
    };
  };
}
