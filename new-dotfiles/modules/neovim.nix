{ inputs, system, ... }:
{
  environment.systemPackages = [
    inputs.nix-exprs.packages."${system}".neovim
  ];

  # Set as default editor
  environment.sessionVariables = {
    EDITOR = "e-wait";
    VISUAL = "e-wait";
  };
}
