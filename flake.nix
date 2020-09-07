{
  description = "My personal dotfiles and configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    nur.url = "git+https://gitlab.com/jD91mZM2/nur-packages.git";
    # nur = {
    #   type = "path";
    #   path = "./nur-packages";
    # };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, nur, emacs-overlay } @ inputs: let
    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
  in {
    overlay = final: prev: (
      builtins.foldl' (a: b: a // b) {} (
        map
          (overlay: overlay final prev)
          self.overlays
      )
    );
    overlays =
      [ emacs-overlay.overlay ]

      # All overlays in the overlays directory
      ++ (
        map
          (name: import (./overlays + "/${name}"))
          (builtins.attrNames (builtins.readDir ./overlays))
      );

    lib = {
      system = forAllSystems (system: let
        pkgs = nixpkgs.legacyPackages."${system}";

        shared = pkgs.callPackage ./shared {};

        configInputs = {
          inherit system;
          modules = [
            # Base packages
            nur.nixosModules.programs
            ./shared/base.nix
          ];
          extraArgs = {
            inherit self shared inputs system;
          };
        };
      in {
        mkNixosConfig = module:
          nixpkgs.lib.nixosSystem (configInputs // {
            modules = configInputs.modules ++ [ module ];
          });

        mkNixosModule = module: {
          # Hacky way to send extraArgs to a module directly
          _module.args = configInputs.extraArgs;
          imports = configInputs.modules ++ [ module ];
        };
      });
    };

    # NixOS configurations
    nixosConfigurations = let
      mkNixosConfig = self.lib.system."x86_64-linux".mkNixosConfig;
    in {
      samuel-computer = mkNixosConfig ./etc/computer/configuration.nix;
      samuel-laptop = mkNixosConfig ./etc/laptop/configuration.nix;
    };

    devShell = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages."${system}";
    in pkgs.mkShell {
      # Things to be put in $PATH
      nativeBuildInputs =
        # Convenient scripts
        [
          (pkgs.writeShellScriptBin "deploy" ''
            "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
          '')
        ];
    });
  };
}
