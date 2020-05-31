{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      pulseSupport = true;
    };
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
        height     = 30;
        padding    = 3;

        override-redirect = true;

        modules-left  = "ewmh_taskbar";
        modules-right = "date pulseaudio battery";
        module-margin = 1;
        tray-position = "right";
      };

      "bar/secondary" = {
        monitor    = "\${env:MONITOR}";
        background = shared.theme.background;
        font-0     = "Hack:size=10;2";
        font-1     = "Font Awesome 5 Free Solid:size=10;2";
        height     = 30;
        padding    = 3;

        override-redirect = true;

        modules-left  = "ewmh_taskbar";
        module-margin = 3;
      };

      "module/ewmh_taskbar" = {
        type                    = "internal/xworkspaces";
        label-active-foreground = shared.theme.purple;
        label-active-background = shared.theme.current-line;
        label-urgent-foreground = shared.theme.red;

        icon-0 = "1;";
        icon-1 = "2;";
        icon-2 = "3;";
        icon-3 = "4;";
        icon-4 = "5;5";
        icon-5 = "6;6";
        icon-6 = "7;7";
        icon-7 = "8;";
        icon-8 = "9;";

        label-active   = "%icon%";
        label-occupied = "%icon%";
        label-urgent   = "%icon%";
        label-empty    = "%icon%";

        label-active-padding   = 2;
        label-occupied-padding = 2;
        label-urgent-padding   = 2;
        label-empty-padding    = 2;
      };

      "module/date" = {
        type             = "internal/date";
        date             = "%b %d";
        time             = "%H:%M";
        label            = "%date% %time%";
        label-background = shared.theme.background;
        interval         = 30;
      };

      "module/pulseaudio" = {
        type          = "internal/pulseaudio";
        format-volume = "<label-volume> <bar-volume>";

        label-muted = "";

        label-volume = " %percentage%%";

        bar-volume-width            = 10;
        bar-volume-empty            = "█";
        bar-volume-empty-foreground = shared.theme.comment;
        bar-volume-fill             = "█";
        bar-volume-indicator        = "";
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
