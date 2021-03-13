{ pkgs, lib, self, system, ... }:

with lib;

let
  # Privacy is good
  chromium = pkgs.chromium;

  mkApp = url: ''"${chromium}/bin/chromium" --app="${url}"'';

  discord = mkApp "https://discord.com/channels/@me/";
  mattermost = mkApp "https://chat.redox-os.org/";

  preferences = {
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
    self.packages."${system}".firefox
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
          #navigator-toolbox {
            display: none 
          }
        '';
      };
    };
  };
}
