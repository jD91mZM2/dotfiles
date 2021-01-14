{
  description = "My personal dotfiles and configurations";

  inputs = {
    nixpkgs-master.url = "nixpkgs/master";
    utils.url = "github:numtide/flake-utils";

    # My stuff
    nix-exprs.url = "gitlab:jD91mZM2/nix-exprs";
    redox-world-map.url = "gitlab:jD91mZM2/redox-world-map";

    # Nix tools
    home-manager.url = "github:nix-community/home-manager";
    nixos-generators.url = "github:jD91mZM2/nixos-generators/flake";
    nixpkgs-fmt.url = "github:nix-community/nixpkgs-fmt";

    # Packages
    crate2nix = { url = "github:kolloch/crate2nix"; flake = false; };
    nur-rycee = { url = "gitlab:rycee/nur-expressions"; flake = false; };
  };

  outputs =
    { self

    , nixpkgs
    , nixpkgs-master
    , utils

    , nix-exprs
    , redox-world-map

    , home-manager
    , nixos-generators
    , nixpkgs-fmt

    , crate2nix
    , nur-rycee
    } @ inputs:

    {
      overlay = final: prev: {
        clangd = (
          let
            clang = final.llvmPackages.clang-unwrapped;
          in
          final.runCommand "clangd-${final.stdenv.lib.getVersion clang}" { } ''
            mkdir -p "$out/bin"
            ln -s "${clang}/bin/clangd" "$out/bin/clangd"
          ''
        );

        # Add rycee's nur packages
        nur-rycee = final.callPackage nur-rycee { };

        # Override or add packages
        nixos-generators = nixos-generators.defaultPackage."${final.system}";
        crate2nix = final.callPackage crate2nix { };
        nixpkgs-fmt = nixpkgs-fmt.defaultPackage."${final.system}";

        neovim = nix-exprs.packages."${final.system}".neovim;
        st = nix-exprs.packages."${final.system}".st;
      };

      # NixOS configurations
      nixosConfigurations = with self.lib."x86_64-linux"; {
        samuel-computer = mkNixosConfigWithHome ./etc/computer/configuration.nix;
        samuel-laptop = mkNixosConfigWithHome ./etc/laptop/configuration.nix;
      };
    } // (utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages."${system}";

      shared = pkgs.callPackage ./shared {
        inherit inputs;
      };

      configInputs = {
        inherit system;
        modules = [
          # Base packages
          nix-exprs.nixosModules.all
        ];
        extraArgs = {
          inherit self shared inputs system;
        };
      };
    in
    rec {
      lib = rec {
        inherit configInputs;

        mkNixosConfig = modules:
          nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem (configInputs // {
            modules = configInputs.modules ++ nixpkgs.lib.toList modules;
          });

        mkNixosConfigWithHome = modules: mkNixosConfig ([
          home-manager.nixosModules.home-manager
        ] ++ nixpkgs.lib.toList modules);

        mkNixosModule = module: {
          # Hacky way to send extraArgs to a module directly
          _module.args = configInputs.extraArgs;
          imports = configInputs.modules ++ [ module ];
        };

        mkHomeModule = module: {
          # Hacky way to send extraArgs to a module directly
          _module.args = configInputs.extraArgs;
          imports = [ module ];
        };
      };

      packages = {
        iso = (lib.mkNixosConfigWithHome ./etc/iso/configuration.nix).config.system.build.isoImage;
      };

      devShell = pkgs.mkShell {
        # Things to be put in $PATH
        nativeBuildInputs =
          # Convenient scripts
          [
            # pkgs.nixops
            # (pkgs.writeShellScriptBin "deploy" ''
            #   "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
            # '')
          ];
      };
    }));
}
