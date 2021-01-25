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
    scaff.url = "gitlab:jD91mZM2/scaff";
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
      coreModules = [
        # Global modules
        ./modules/globals

        # Need to be placed here to avoid infinite recursion
        inputs.nix-exprs.nixosModules.zsh-vi
        inputs.nix-exprs.nixosModules.powerline-rs

        # Home-manager modules
        inputs.home-manager.nixosModules.home-manager
      ];
      args = {
        inherit self inputs;
      };
    in
    {
      lib = {
        makeModule = system: modules: {
          imports = coreModules ++ toList modules;
          _module.args = args // {
            inherit system;
          };
        };
        makeSystem = system: modules: nixosSystem {
          inherit system;
          modules = coreModules ++ toList modules;
          extraArgs = args // {
            inherit system;
          };
        };
      };

      # NixOS configurations
      nixosConfigurations = {
        samuel-computer = self.lib.makeSystem "x86_64-linux" ./systems/personal-computer;
      };
    } // (utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";

        # Used by neovim which relies on tree-sitter which apparently is marked as "broken" in my version of nixpkgs
        broken-pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
      in
      {
        packages = {
          neovim = broken-pkgs.callPackage ./pkgs/neovim {
            inherit inputs;
          };
          st = inputs.st.defaultPackage."${system}";

          iso = (makeSystem "x86_64-linux" ./systems/iso).config.system.build.isoImage;
        };

        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ terraform_0_14 ];
        };
      }));
}
