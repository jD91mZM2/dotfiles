{ ... }:

{
  imports = [
    # Include setup that's generic across my devices
    ../../generic
  ];

  setup = {
    name = "vm-gui";

    graphics.enable = true;
    packages.languages.wasm = true;
  };
}
