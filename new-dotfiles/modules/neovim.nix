{ pkgs, inputs, system, ... }:
let
  # My favorite terminal
  st = inputs.st.packages."${system}".st;

  # Pre-configured neovim binary
  neovim = inputs.nix-exprs.packages."${system}".neovim;
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
