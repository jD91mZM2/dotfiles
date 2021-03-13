{ firefox }:

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

firefox.override {
  extraPolicies = {
    Cookies = {
      AcceptThirdParty = "never";
      Locked = true;
    };

    DisableTelemetry = true;
    DisableFirefoxStudies = true;

    PasswordManagerEnabled = false;

    SearchEngines = {
      Default = "DuckDuckGo";
      PreventInstalls = true;
    };

    EnableTrackingProtection = {
      Value = true;
      Locked = true;
      Cryptomining = true;
      Fingerprinting = true;
    };

    Preferences = {
      "browser.shell.checkDefaultBrowser" = preference false;
      "browser.aboutConfig.showWarning" = preference false;

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
      "media.eme.enabled" = preference false;
      "media.gmp-widevinecdm.enabled" = preference false;
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
  };
}
