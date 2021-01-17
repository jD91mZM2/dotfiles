{
  description = "A new WIP configuration to replace my dotfiles";

  inputs = {
    # Misc
    utils.url = "github:numtide/flake-utils";
    st.url = "gitlab:jD91mZM2/st";

    # NixOS Modules
    home-manager.url = "github:nix-community/home-manager";
    nix-exprs.url = "gitlab:jD91mZM2/nix-exprs";

    # Required by modules
    neovim-nightly = {
      url = "github:neovim/neovim/nightly";
      flake = false;
    };
    nur-rycee = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, ... } @ inputs:
    with nixpkgs.lib;
    let
      makeSystem = system: modules: nixosSystem {
        inherit system;
        modules = toList modules ++ [
          # Global modules
          ./modules/globals

          # Need to be placed here to avoid infinite recursion
          inputs.nix-exprs.nixosModules.zsh-vi
          inputs.nix-exprs.nixosModules.powerline-rs

          # Home-manager modules
          inputs.home-manager.nixosModules.home-manager
        ];
        extraArgs = {
          inherit self inputs system;
        };
      };
    in
    {
      # NixOS configurations
      nixosConfigurations = {
        samuel-computer = makeSystem "x86_64-linux" [ ./systems/personal-computer ];
      };
    } // (utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
      in
      {
        packages = {
          neovim = pkgs.callPackage ./pkgs/neovim {
            inherit inputs;
          };
          st = inputs.st.defaultPackage."${system}";
        };
      }));
}
