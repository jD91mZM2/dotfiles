{ pkgs, ... }:

{
  # Configs
  programs.firefox = {
    enable = true;
    privacy.extensions.enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      # Other addons
      bitwarden
      stylus
      vimium
    ];
    profiles.main = {
      id = 0;
      name = "home-manager";
      privacy.enableSettings = true;
      settings = {
        # Fix issues with having a dark GTK theme
        "ui.use_standins_for_native_colors"   = true;
        "widget.content.allow-gtk-dark-theme" = false;
        "widget.chrome.allow-gtk-dark-theme"  = false;
        "widget.content.gtk-theme-override"   = "Adwaita:light";

        # Disable WebRTC because it's scary
        "media.peerconnection.enabled" = false;
      };
    };
  };
}
