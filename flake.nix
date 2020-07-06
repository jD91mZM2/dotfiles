{
  description = "My personal dotfiles and configurations";

  inputs = {
    nur = {
      type = "path";
      path = "./nur-packages";
    };
    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
    };
  };

  outputs = { self, nixpkgs, nur, emacs-overlay } @ inputs: let
    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
  in {
    packages = forAllSystems (system: let
      shared = nixpkgs.legacyPackages."${system}".callPackage ./shared {};
      sharedBase = ./shared/base.nix;

      mkNixosConfig = config: nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          # Base packages
          nur.nixosModules.programs
          ./shared/base.nix

          # Config
          config
        ];
        extraArgs = inputs // {
          inherit self shared sharedBase;
        };
      };
    in {
      nixosConfigurations = {
        samuel-computer = mkNixosConfig ./etc/computer/configuration.nix;
      };
    });

    overlays =
      [ emacs-overlay.overlay ]
      ++ (
        map
          (name: import (./overlays + "/${name}"))
          (builtins.attrNames (builtins.readDir ./overlays))
      );
  };
}
