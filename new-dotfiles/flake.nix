{
  description = "A new WIP configuration to replace my dotfiles";

  inputs = {
    home-manager.url = "github:nix-community/home-manager";

    # Required by modules
    st.url = "gitlab:jD91mZM2/st";
    nix-exprs.url = "gitlab:jD91mZM2/nix-exprs";
    nur-rycee = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    with nixpkgs.lib;
    let
      makeSystem = system: modules: nixosSystem {
        inherit system;
        modules = toList modules ++ [
          ./modules/globals
          inputs.home-manager.nixosModules.home-manager
        ];
        extraArgs = {
          inherit inputs system;
        };
      };
    in
    {
      # NixOS configurations
      nixosConfigurations = {
        samuel-computer = makeSystem "x86_64-linux" [ ./systems/personal-computer ];
      };
    };
}
