{
  description = "My personal dotfiles and configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-master.url = "nixpkgs/master";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nur-rycee = { url = "git+https://gitlab.com/rycee/nur-expressions.git"; flake = false; };

    nur.url = "git+https://gitlab.com/jD91mZM2/nur-packages.git";
    # nur.url = "./nur-packages.git";
    redox-world-map.url = "git+https://gitlab.com/jD91mZM2/redox-world-map.git";
    # redox-world-map.url = "/home/user/Coding/Rust/redox-world-map";
  };

  outputs = { self, nixpkgs, nixpkgs-master, nur-rycee, emacs-overlay, nur, redox-world-map } @ inputs: let
    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
  in {
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

      nur-rycee = prev.callPackage nur-rycee {};

      mkchromecast = nixpkgs-master.legacyPackages."${prev.system}".mkchromecast;
    };

    lib = {
      system = forAllSystems (system: let
        pkgs = nixpkgs.legacyPackages."${system}";

        shared = pkgs.callPackage ./shared {
          inherit inputs;
        };

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

        mkHomeModule = module: {
          # Hacky way to send extraArgs to a module directly
          _module.args = configInputs.extraArgs;
          imports = [ module ];
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
          pkgs.nixops
          (pkgs.writeShellScriptBin "deploy" ''
            "${pkgs.nixops}/bin/nixops" deploy -d main --check --allow-reboot "$@"
          '')
        ];
    });
  };
}
