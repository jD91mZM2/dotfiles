let
  flake = import <dotfiles>;
  mkHomeModule = flake.lib.system."${builtins.currentSystem}".mkHomeModule;
in mkHomeModule ({
  imports = [
    ./generic
  ];

  setup = {
    source = ./full.nix;

    env.enable = true;
    firefox.enable = true;
    graphics.enable = true;

    shells.enableZsh = true;
    shells.enableBash = true;
    shells.enableGit = true;
  };
})
