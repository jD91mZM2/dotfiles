{ ... }:

{
  imports = [
    # Include setup that's generic across my devices
    ../generic
    # ../../shared/base.nix
  ];

  setup = {
    graphics = {
      enable = true;
      fast   = true;
    };
    packages.languages.wasm = true;
  };
}
