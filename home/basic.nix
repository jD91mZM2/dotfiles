let
  flake = import <dotfiles>;
  mkHomeModule = flake.lib.system."${builtins.currentSystem}".mkHomeModule;
in mkHomeModule ({
  imports = [
    ./generic
  ];
  setup = {
    source = ./basic.nix;
    shells.enableZsh = true;
  };
})
