{ ... }:

{
  imports = [
    # Include setup that's generic across my devices
    ../generic
  ];

  setup = {
    name = "vm-headless";

    packages.languages.wasm = true;
  };
}
