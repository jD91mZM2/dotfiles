{ config, lib, inputs, system, ... }:
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

    # Input drivers
    libinput.enable = true;
  };

  # Base packages
  environment.systemPackages = [
    inputs.st.defaultPackage."${system}"
  ];

  home = {
    # Set my colourscheme in xresources
    xresources.properties = (
      (
        listToAttrs (
          imap0
            (index: colour: (
              nameValuePair "*.color${toString index}" "#${colour}"
            ))
            config.globals.colourscheme.xresources
        )
      ) // {
        "*.background" = "#${builtins.elemAt config.globals.colourscheme.colours 0}";
        "*.foreground" = "#${builtins.elemAt config.globals.colourscheme.colours 5}";
      }
    );
  };
}
