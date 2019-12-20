{ ... }:

{
  imports = [
    # Include results from hardware scan
    ./hardware-configuration.nix

    # Include setup that's generic across my devices
    ../generic
  ];

  setup = {
    name      = "@@DEVICE_NAME@@";
    networkId = "@@NETWORK_ID@@";
  };
}
