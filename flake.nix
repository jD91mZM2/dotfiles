{
  description = "My personal dotfiles and configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs-channels/nixos-unstable";

    nur = {
      type = "path";
      path = "./nur-packages";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixops.url = "nixops";
    nixops-digitalocean = {
      url = "github:jD91mZM2/nixops-digitalocean";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nur, emacs-overlay, nixops, nixops-digitalocean } @ inputs: let
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

    packages = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages."${system}";

      shared = pkgs.callPackage ./shared {};
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
      # NixOS configurations
      nixosConfigurations = {
        samuel-computer = mkNixosConfig ./etc/computer/configuration.nix;
        samuel-laptop = mkNixosConfig ./etc/laptop/configuration.nix;
      };

      # Packages
      nixops = nixops.packages."${system}".nixops.overridePythonAttrs (attrs: {
        propagatedBuildInputs = attrs.propagatedBuildInputs ++ [
          (pkgs.callPackage nixops-digitalocean {})
        ];
      });
    });

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

    nixopsConfigurations.default = {
      network.description = "My personal VPS network";
      resources.sshKeyPairs.ssh-key = {};

      inherit nixpkgs;

      main = import ./servers/main;
    };
  };
}
