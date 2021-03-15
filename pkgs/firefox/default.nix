{ pkgs }:

with pkgs.lib;

let
  preference = val: {
    Value = val;
    Status = "locked";
  };

  extension = name: {
    installation_mode = "force_installed";
    install_url = "https://addons.mozilla.org/firefox/downloads/latest/${name}/latest.xpi";
  };
in

pkgs.firefox.override {
  extraPolicies = {
    Cookies = {
      AcceptThirdParty = "never";
      Locked = true;
    };

    DisableTelemetry = true;
    DisableFirefoxStudies = true;

    PasswordManagerEnabled = false;

    EnableTrackingProtection = {
      Value = true;
      Locked = true;
      Cryptomining = true;
      Fingerprinting = true;
    };

    Preferences = {
      "browser.shell.checkDefaultBrowser" = preference false;
      "browser.aboutConfig.showWarning" = preference false;

      # I need widevine for stuff, sorry privacy :(
      "media.gmp-widevinecdm.enabled" = preference true;
      "media.eme.enabled" = preference true;

      # Support installing xpi from sources outside of mozilla
      "extensions.install.requireBuiltInCerts" = preference false;

      # Preferences according to https://www.privacytools.io/browsers/#about_config
      # {{{
      # Cannot change privacy.* here

      # browser.*
      "browser.send_pings" = preference false;
      "browser.urlbar.speculativeConnect.enabled" = preference false;
      "browser.sessionstore.privacy_level" = preference 2;
      "browser.safebrowsing.downloads.remote.enabled" = preference false;

      # dom.*
      "dom.event.clipboardevents.enabled" = preference false;

      # media.*
      "media.navigator.enabled" = preference false;

      # network.*
      "network.cookie.cookieBehavior" = preference 1;
      "network.http.referer.XOriginPolicy" = preference 2;
      "network.http.referer.XOriginTrimmingPolicy" = preference 2;
      "network.IDN_show_punycode" = preference true;

      # Disable prefetch
      "network.dns.disablePrefetch" = preference true;
      "network.dns.disablePrefetchFromHTTPS" = preference true;
      "network.predictor.enabled" = preference false;
      "network.predictor.enable-prefetch" = preference false;
      "network.prefetch-next" = preference false;

      # Cannot change webgl.* here

      # Cannot change beacon.* here
      # }}}
    };

    ExtensionSettings = {
      "*" = {
        installation_mode = "blocked";
        blocked_install_message = "How could you possibly forget that you manage add-ons via Nix? Duh";
      };
      "queryamoid@kaply.com" = {
        installation_mode = "force_installed";
        install_url = "https://github.com/mkaply/queryamoid/releases/download/v0.2/query_amo_addon_id-0.2-fx.xpi";
      };

      # General addons
      "jid1-KKzOGWgsW3Ao4Q@jetpack" = extension "i-dont-care-about-cookies";
      "{446900e4-71c2-419f-a6a7-df9c091e268b}" = extension "bitwarden-password-manager";
      "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}" = extension "styl-us";
      "vimium-c@gdh1995.cn" = extension "vimium-c";
      "{b743f56d-1cc1-4048-8ba6-f9c2ab7aa54d}" = extension "dracula-dark-colorscheme";
      "firenvim@lacamb.re" = extension "firenvim";

      # Add-ons as suggested by https://www.privacytools.io/browsers/#addons
      "uBlock0@raymondhill.net" = extension "ublock-origin";
      "https-everywhere@eff.org" = extension "https-everywhere";
      "jid1-BoFifL9Vbdl2zQ@jetpack" = extension "decentraleyes";
      "{74145f27-f039-47ce-a470-a662b129930a}" = extension "clearurls";

      # Other security add-ons
      "jid1-MnnxcxisBPnSXQ@jetpack" = extension "privacy-badger17";
    };

    SearchEngines = {
      Default = "searx";
    };

    DisableProfileImport = true;

    DisplayBookmarksToolbar = true;
    ManagedBookmarks = [
      {
        name = "NixOS";
        children = [
          { name = "IRC Chat Log"; url = "https://logs.nix.samueldr.com/nixos-chat/"; }
          { name = "Builtins"; url = "https://nixos.org/nix/manual/#ssec-builtins"; }
          { name = "Nixpkgs manual"; url = "https://nixos.org/nixpkgs/manual"; }
          { name = "NixOS manual"; url = "https://nixos.org/nixos/manual"; }
        ];
      }
      {
        name = "Rust";
        children = [
          { name = "Standard Library"; url = "https://doc.rust-lang.org/std"; }
          { name = "Crate Search"; url = "https://docs.rs/"; }
        ];
      }
      {
        name = "Python";
        children = [
          { name = "Modules"; url = "https://docs.python.org/3/search.html"; }
          { name = "Builtins"; url = "https://docs.python.org/3/library/functions.html#built-in-funcs"; }
          {
            name = "Discord.py";
            url = "https://discordpy.readthedocs.io/en/latest/api.html";
            children = [
              { name = "API Documentation"; url = "https://discordpy.readthedocs.io/en/latest/api.html"; }
              { name = "Bot Commands"; url = "https://discordpy.readthedocs.io/en/latest/ext/commands/index.html"; }
            ];
          }
        ];
      }
      {
        name = "Haskell";
        children = [
          { name = "Hoogle Search"; url = "https://www.haskell.org/hoogle/"; }
        ];
      }
      {
        name = "Setup Searx";
        url = "https://search.modalogi.com/preferences?preferences=eJx1VUuv0zoQ_jU3m6joAgtWWSDuhYOEBOIUttbEnjqjOJ4wdtoTfj12kzTJOWXRqB7P45tvHtYQ0bIQhsqiRwFXOPB2AIsV-sOPx8KxBpcPBQyRNXe9w4iVGXSbf5YL6pK26oWfxuooAxYdxoZN9en_YxHghAFBdFP9W8QGO6wCZReFYBhcDIq98nhREerqI7iAhWFS6ZLdGaViSMdXLLbgoEEOIY4JjGNLmg2eDwakLcCcwWs0ao40-6EAtUtS9JZ8SjCyCPr4W6luCKT_efNBa32IZ6XOZJBDEpwEsQx8ihcQLA0J6mQ1KkUx3xphMkqdyGHWtszWoVIzc0lyoZYS7EBx3IonXBlxmT-zu_y3RrHzMUSQ2GcqN5aR25Ejh4Zb8BugjusQ8ZWE2RgjswvPofwaOO7cvX5K-rlc2QdoG3jYOH0WbElz0VtYy55TyVK0_J0FSUP0LtbaIeUUchPajwCrw18X8PEFjTw-4-LC7iTQgesb2Mq5Ry_Yc1ghm8Gg3-oIGkPxhrhHlDjUuEke-rbsSIRlpnRtl8VrA7VA_swaI3iDT9swPUkaqBrG1ehC9a4XavL2HrJVMkLDvBWchHxLoDcEWrKpuSHsDCdWbxltwNxkOXo5nTbSyXBBfC3Hy5rpIfOhx3vgb0pTgaiWpcmX5lmRI7aROlw6lzy8QHJTRuFLnrgNfdGQtVt6WzIQ4Z7OPSaWnjPGlgZP5CkS-93k-L6bsclQjxa7BWqOVTO3YV_PWKcux7gqZY8gO6Iwjh17l-ZmKx186FMRmztDueS3duEtm9dv3757-vvYXjMs0O-X37SryhRuLKHvw_MtNt1cbVeSLEUH9ZzYTo_PhNsOWhohsCZwZYeGYN3AvRsSilD9pO7gqEXVcGxxzIaf_bUGqIIWdpmUr2mc1XutMQT139fP6S24SFK4ZbQ4e8jETYdkdRRIRRD14_uXZJHwoSTp43Xx5jdGTw_dmN4Il5b69c6dVArP0sG1B5Ls4Xj89niLmIjFFOgPuX2xdA==";
      }
    ];
  };
}
