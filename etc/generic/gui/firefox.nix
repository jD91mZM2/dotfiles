{ pkgs, config, lib, ... }:

let
  cfg = config.setup.graphics.firefox;
in
{
  options.setup.graphics.firefox = {
    enable = lib.mkEnableOption "firefox";
  };

  config = lib.mkIf cfg.enable {
    setup.home.modules = lib.singleton {
      programs.firefox = {
        enable = true;
        privacy.extensions = {
          enable = true;
          repository = pkgs.nur-rycee.firefox-addons;
        };
        extensions = with pkgs.nur-rycee.firefox-addons; [
          bitwarden
          stylus
          vimium
        ];
        profiles.main = {
          id = 0;
          name = "home-manager";
          privacy.settings = {
            enable = true;
            exceptions = [
              # Enable WebGL because I need it :(
              "webgl.disabled"
            ];
          };
          settings = {
            # Fix issues with having a dark GTK theme
            "ui.use_standins_for_native_colors" = true;
            "widget.content.allow-gtk-dark-theme" = false;
            "widget.chrome.allow-gtk-dark-theme" = false;
            "widget.content.gtk-theme-override" = "Adwaita:light";

            # Disable WebRTC because it's scary
            "media.peerconnection.enabled" = false;
          };
        };
      };
    };
  };
}
