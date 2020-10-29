{ ... }:

{
  imports = [
    # Include setup that's generic across my devices
    ../generic
  ];

  setup = {
    name = "vm-headless";

    bootloader = false;
    packages.languages.wasm = true;
  };

  hyperv.baseImageSize = 8192;
}
