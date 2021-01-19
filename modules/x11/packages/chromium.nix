{ pkgs, lib, ... }:
with lib;
let
  # Privacy is good
  chromium = pkgs.chromium;

  mkApp = url: ''"${chromium}/bin/chromium" --app="${url}"'';

  discord = mkApp "https://discord.com/channels/@me/";
  mattermost = mkApp "https://chat.redox-os.org/";
in
{
  environment.systemPackages = [
    chromium

    (pkgs.makeDesktopItem {
      name = "discord";
      desktopName = "Discord";
      exec = discord;
    })
    (pkgs.makeDesktopItem {
      name = "redox-mattermost";
      desktopName = "Redox Mattermost";
      exec = mattermost;
    })
  ];

  programs.chromium = {
    enable = true;

    defaultSearchProviderSearchURL = "https://duckduckgo.com/?q={searchTerms}";

    extraOpts = {
      # Hack: NixOS should set this, see https://github.com/NixOS/nixpkgs/pull/109913
      DefaultSearchProviderEnabled = true;

      # Enable good options
      BlockThirdPartyCookies = true;
      BookmarkBarEnabled = true;

      # Disable bad options
      ShowAppsShortcutInBookmarkBar = false;
      PasswordManagerEnabled = false;
    };

    extensions = [
      "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
      "clngdbkpkpeebahjckkjfobafhncgmne" # Stylus
      "fihnjjcciajhdojfnbdddfaoknhalnja" # I don't care about cookies
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
      "bmnlcjabgnpnenekpadlanbbkooimhnj" # Honey
      "gfapcejdoghpoidkfodoiiffaaibpaem" # Dracula Theme
      "djflhoibgkdhkhhcedjiklpkjnoahfmg" # User Agent Switcher

      # Extensions as recommended by https://www.privacytools.io/browsers/#addons
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # Privacy Badger
      "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
      "ldpochfccmkkmhdbclfhpagapcfdljkj" # Decentraleyes
      "lckanjgmijmafbedllaakclkaicjfmnk" # ClearURLs

      "hjdoplcnndgiblooccencgcggcoihigg" # tosdr
    ];
  };

  # Startup
  home.xsession.initExtra = ''
    ${discord} &
    ${mattermost} &
  '';
}
