{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Editor stuff
    nixpkgs-fmt
    rnix-lsp

    # Pushing cache to cachix
    cachix

    # Scripts
    nix-prefetch-scripts
    nix-review

    # Searching for packages by content
    nix-index
  ];
}
