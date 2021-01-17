{ inputs, system, ... }:
{
  environment.systemPackages = [
    inputs.nix-exprs.packages."${system}".neovim
  ];
}
