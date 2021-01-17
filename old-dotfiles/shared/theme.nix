{ pkgs, inputs }:

inputs.nix-exprs.lib.base16.dracula // {
  gtkTheme = {
    name = "Dracula";
    package = pkgs.dracula-theme;
  };
  iconTheme = {
    name = "Dracula";
    package = pkgs.dracula-theme;
  };
}
