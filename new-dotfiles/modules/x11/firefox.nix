{ pkgs, config, lib, inputs, ... }:

let
  nur-rycee = import inputs.nur-rycee {
    inherit pkgs;
  };
in
{
  home = {
    imports = [
      inputs.nix-exprs.hmModules.firefox-privacy
    ];

    programs.firefox = {
      enable = true;
      privacy.extensions = {
        enable = true;
        repository = nur-rycee.firefox-addons;
      };
      extensions = with nur-rycee.firefox-addons; [
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
}
