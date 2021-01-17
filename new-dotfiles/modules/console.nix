{ pkgs, config, ... }:

{
  console = {
    font = "Lat2-Terminus16";
    keyMap = config.globals.keyboard.variant;
    colors = config.globals.colourscheme.xresources;
  };
}
