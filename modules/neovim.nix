{ pkgs, self, system, ... }:
let
  neovim = self.packages."${system}".neovim;
in
{
  # Add the package
  environment.systemPackages = [
    neovim
  ];

  # Set as default editor
  environment.sessionVariables = {
    EDITOR = "e-wait";
    VISUAL = "e-wait";
  };
}
