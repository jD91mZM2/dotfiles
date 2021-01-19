{ pkgs, config, lib, self, system, ... }:
with lib;
{
  services.xserver = {
    # Enable X server
    enable = true;

    # Add way to kill if unresponsive
    enableCtrlAltBackspace = true;

    # Set my keyboard layout
    layout = config.globals.keyboard.layout;
    xkbVariant = config.globals.keyboard.variant;

    # Add composite key
    xkbOptions = "compose:ralt";

    # Input drivers
    libinput.enable = true;
  };

  # All you really need is a terminal
  environment.systemPackages = [
    self.packages."${system}".st
  ];

  # Hack font for monospace (used by st terminal)
  fonts = {
    fonts = with pkgs; [ hack-font ];
    fontconfig.defaultFonts = {
      monospace = [ "Hack" ];
    };
  };

  # Set my colourscheme in xresources
  home = [
    {
      xresources.properties = (
        listToAttrs (
          imap0
            (index: colour: (
              nameValuePair "*.color${toString index}" "#${colour}"
            ))
            config.globals.colourscheme.xresources
        )
      );
    }
    {
      xresources.properties = {
        "*.background" = "#${builtins.elemAt config.globals.colourscheme.colours 0}";
        "*.foreground" = "#${builtins.elemAt config.globals.colourscheme.colours 5}";
        "*.font" = "Hack:pixelsize=13:antialias=true:autohint=true";
      };
    }
  ];
}
