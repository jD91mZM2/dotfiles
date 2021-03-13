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
        name = "Searx";
        url = "https://searx.ir/?preferences=eJx9VcGO3DYM_Zr4YmyRNIecfCiatA0QIEF306tBSxwNY0l0JHlmla8vNTO25ewgh52FaPLxkXyiFCQ0HAhjZ9BjAPvq9z8pNRa8mcFgh_7h62NjWYEthwbmxIrdZDFhp2c1lj_DDTnx7qfAz7n7C2zExmE6su7-_vDURDhgRAjq2L1u0hEddpEKRhMwzjbFnn3v8dwnGG7RmqmXj2xPGDoGOf7GwTQcFYSHmLKwsWxIscZToynCYFH36A15qQUTs419v9WUeMycOB55BN_3B7IYxVw40Qlb2jlj4DPp2mLISAaIqTZG_OHB1ZatIe2lIYJ6_S_fLA0BQm6Ld9znM8zGYjtZyK3jE5W4E2nkEnfQgQuZhXJArSlVwGcaaWAed5DFGHkOCmvrQGkQeljCU4UlvS9IWptW44E8JWL_Au_7zAl_0VQ3R1KFcSA_EqiK45vn6nBikJyRFYFtHWqCIjoPVdGJQ0Cffmyg2ZibcWtF3TeYplh9QRwTucsA6kI37keQcZSfm0eNv-QEZaSHL89Llgxe4_NeEqgnxLD5RJwIruKv6huSJmMqg89Q1190PWAwN3Kl-3IRIqVcZ-MJfcCJq8IXimuzMWXH3sq1qCOv6VanQwAHRaB4S3glXFi05edOj1aqwq2oRaRdJ1BKPaRT5Xab1fV8d9I1IhVFrrocSY0Qd5ciILaRD-kMAVtNAZUA5RvPDXNpy0RBNt0A-b54LgPe5jyQNy-ZXobAuey5qtAKefUsCkjzgJXJZYdOGLYpgI9WQvTLeSz5wzxkg24Rb5XiZyWu-N_P4Hf6Xnu21Ktnjb72IC_RxHNdoyxtqS_cl_SS3MNp7_Lm7dt3z3fUvaS-VrcRWZZkjZHhyPyL5bKCr4UtdBx_k9u-o5wgpKkeVIN-_0IsC-GnLWQoWRhK37dHZbKzxMRuxVgM_zw9fXmUV-ocKKEE_0fuwdKI_ZHTiLmQfbxefHne1PWlzX1EK2KVbx_9ZdNiH1VgW3h_lvvc_6EUSnnvP3-soB_RHnoJ4ODgspvF9hRAVnnov_77STzl3cDQSNNQmP0PxdD10g==";
      }
    ];
  };
}
