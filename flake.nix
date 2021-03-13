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
    inc.url = "gitlab:jD91mZM2/inc";

    # Required by neovim
    ale = { url = "github:dense-analysis/ale"; flake = false; };
    neovim-nightly = { url = "github:neovim/neovim/nightly"; flake = false; };
    nvim-treesitter = { url = "github:nvim-treesitter/nvim-treesitter"; flake = false; };
    vim-mcfunction.url = "gitlab:jD91mZM2/vim-mcfunction";

    # Required by server
    abottomod.url = "gitlab:jD91mZM2/abottomod";
    schedulebot.url = "gitlab:jD91mZM2/schedulebot";
    timeywimey.url = "gitlab:jD91mZM2/timeywimey";

    mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
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

        # Needed by server
        inputs.nix-exprs.nixosModules.custom-services
        inputs.mailserver.nixosModules.mailserver
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
        samael-computer = self.lib.makeSystem "x86_64-linux" ./systems/personal-computer;
        samael-laptop = self.lib.makeSystem "x86_64-linux" ./systems/laptop;
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
          st = inputs.st.defaultPackage."${system}";

          neovim = broken-pkgs.callPackage ./pkgs/neovim {
            inherit inputs;
          };
          firefox = import ./pkgs/firefox {
            # Can't use callPackage because that replaces `.override` which we
            # need to retain.
            inherit pkgs;
          };

          iso = (self.lib.makeSystem "x86_64-linux" ./systems/iso).config.system.build.isoImage;
        };

        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            morph
            terraform_0_14
          ];
        };
      }));
}
