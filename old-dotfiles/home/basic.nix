let
  flake = import <dotfiles>;
  inherit (flake.lib."${builtins.currentSystem}") mkHomeModule;
in
mkHomeModule ({
  imports = [
    ./generic
  ];
  setup = {
    source = ./basic.nix;
    shells.enableZsh = true;
  };
})
