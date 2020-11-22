{
  description = "My personal dotfiles and configurations";

  inputs = {
    nixpkgs-master.url = "nixpkgs/master";
    utils.url = "github:numtide/flake-utils";

    nix-exprs.url = "gitlab:jD91mZM2/nix-exprs";
    st.url = "gitlab:jD91mZM2/st";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    nixos-generators.url = "github:jD91mZM2/nixos-generators/flake";
    redox-world-map.url = "git+https://gitlab.com/jD91mZM2/redox-world-map.git";

    nur-rycee = { url = "git+https://gitlab.com/rycee/nur-expressions.git"; flake = false; };

    # Packages
    crate2nix = { url = "github:kolloch/crate2nix"; flake = false; };
  };

  outputs =
    { self

    , nixpkgs
    , nixpkgs-master
    , utils

    , nix-exprs
    , st

    , emacs-overlay
    , home-manager
    , nixos-generators
    , redox-world-map

    , nur-rycee

    , crate2nix
    } @ inputs:

    {
      overlay = final: prev: (emacs-overlay.overlay final prev) // {
        clangd = (
          let
            clang = final.llvmPackages.clang-unwrapped;
          in
            final.runCommand "clangd-${final.stdenv.lib.getVersion clang}" {} ''
            mkdir -p "$out/bin"
            ln -s "${clang}/bin/clangd" "$out/bin/clangd"
          ''
        );

        # Add rycee's nur packages
        nur-rycee = prev.callPackage nur-rycee {};

        # Override or add packages
        mkchromecast = nixpkgs-master.legacyPackages."${prev.system}".mkchromecast;
        nixos-generators = nixos-generators.defaultPackage."${prev.system}";
        crate2nix = prev.callPackage crate2nix {};

        st = st.defaultPackage."${prev.system}";
      };

      # NixOS configurations
      nixosConfigurations = with self.lib."x86_64-linux"; {
        samuel-computer = mkNixosConfigWithHome [ ./etc/computer/configuration.nix ];
        samuel-laptop = mkNixosConfigWithHome [ ./etc/laptop/configuration.nix ];

        samuel-vm-gui = mkNixosConfigWithHome [ ./etc/vm/vm-gui.nix ];
        samuel-vm-headless = mkNixosConfigWithHome [ ./etc/vm/vm-headless.nix ];
      };
    } // (utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages."${system}";

      shared = pkgs.callPackage ./shared {
        inherit inputs;
      };

      configInputs = {
        inherit system;
        modules = [
          # Base packages
          nix-exprs.nixosModules.all
          ./shared/base.nix
        ];
        extraArgs = {
          inherit self shared inputs system;
        };
      };
    in rec {
      lib = rec {
        inherit configInputs;

        mkNixosConfig = modules:
          nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem (configInputs // {
            modules = configInputs.modules ++ modules;
          });

        mkNixosConfigWithHome = modules: mkNixosConfig ([
          home-manager.nixosModules.home-manager
        ] ++ modules);

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

      devShell = pkgs.mkShell {
        # Things to be put in $PATH
        nativeBuildInputs =
          # Convenient scripts
          [
            pkgs.nixops
            (pkgs.writeShellScriptBin "deploy" ''
            "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
          '')
          ];
      };
    }));
}
