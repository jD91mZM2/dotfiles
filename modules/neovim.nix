{ pkgs, self, system, ... }:
let
  # My favorite terminal
  st = self.packages."${system}".st;

  # Pre-configured neovim binary
  neovim = self.packages."${system}".neovim;
in
{
  # Add the package
  environment.systemPackages = [
    neovim

    # Desktop item
    (pkgs.makeDesktopItem {
      name = "neovim";
      desktopName = "neovim gui";
      exec = "${st}/bin/st e";
    })
  ];

  # Set as default editor
  environment.sessionVariables = {
    EDITOR = "e-wait";
    VISUAL = "e-wait";
  };
}
