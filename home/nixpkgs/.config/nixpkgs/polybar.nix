{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  services.polybar = {
    enable = true;
    script = ''
      readarray monitors < <(${pkgs.xorg.xrandr}/bin/xrandr | ${pkgs.gawk}/bin/awk '/\<connected\>/ { print $1 }')
      monitors[0]="$(echo -n ''${monitors[0]})"
      monitors[1]="$(echo -n ''${monitors[1]})"
      MONITOR="''${monitors[0]}" polybar main &
      if [ -n "''${monitors[1]}" ]; then
        MONITOR="''${monitors[1]}" polybar secondary &
      fi
    '';
    config = {
      "bar/main" = {
        monitor    = "\${env:MONITOR}";
        background = shared.theme.background;
        font-0     = "Hack:size=10;2";
        font-1     = "Font Awesome 5 Free Solid:size=10;2";
        wm-restack = "bspwm";
        height     = 30;
        padding    = 3;

        modules-left  = "bspwm";
        modules-right = "date alsa battery";
        module-margin = 1;
        tray-position = "right";
      };

      "bar/secondary" = {
        monitor    = "\${env:MONITOR}";
        background = shared.theme.background;
        font-0     = "Hack:size=10;2";
        font-1     = "Font Awesome 5 Free Solid:size=10;2";
        wm-restack = "bspwm";
        height     = 30;
        padding    = 3;

        modules-left  = "bspwm";
        module-margin = 3;
      };

      "module/bspwm" = {
        type                     = "internal/bspwm";
        label-focused-foreground = shared.theme.purple;
        label-focused-background = shared.theme.current-line;
        label-urgent-foreground  = shared.theme.red;

        ws-icon-0 = "Terminal;";
        ws-icon-1 = "Editor;";
        ws-icon-2 = "Primary;";
        ws-icon-3 = "Secondary;";
        ws-icon-4 = "5;5";
        ws-icon-5 = "6;6";
        ws-icon-6 = "7;7";
        ws-icon-7 = "Misc;";
        ws-icon-8 = "Message;";

        label-empty    = "";
        label-focused  = "%icon%";
        label-urgent   = "%icon%";
        label-occupied = "%icon%";

        label-monitor-padding     = 2;
        label-dimmed-padding      = 2;
        label-focused-padding     = 2;
        label-occupied-padding    = 2;
        label-urgent-padding      = 2;
        label-empty-padding       = 2;
        label-monocle-padding     = 2;
        label-tiled-padding       = 2;
        label-fullscreen-padding  = 2;
        label-floating-padding    = 2;
        label-pseudotiled-padding = 2;
        label-locked-padding      = 2;
        label-sticky-padding      = 2;
        label-private-padding     = 2;
      };

      "module/date" = {
        type             = "internal/date";
        date             = "%b %d";
        time             = "%I:%M %p";
        label            = "%date% %time%";
        label-background = shared.theme.background;
        interval         = 30;
      };

      "module/alsa" = {
        type          = "internal/alsa";
        format-volume = "<ramp-volume>";

        label-muted = "";

        bar-volume-width            = 10;
        bar-volume-empty            = "█";
        bar-volume-empty-foreground = shared.theme.comment;
        bar-volume-fill             = "█";
        bar-volume-indicator        = "";

        ramp-volume-0 = "";
        ramp-volume-1 = "";
        ramp-volume-2 = "";
      };

      "module/battery" = {
        type               = "internal/battery";
        format-charging    = "<animation-charging> <label-charging>";
        format-discharging = "<ramp-capacity> <label-discharging>";

        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";

        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
      };
    };
  };
}
