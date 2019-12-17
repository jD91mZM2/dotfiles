{ ... }:

{
  imports = [
    # Include results from hardware scan
    ./hardware-configuration.nix

    # Include setup that's generic across my devices
    ../generic
  ];

  setup = {
    networkId = "@@NETWORK_ID@@";
  };
}
