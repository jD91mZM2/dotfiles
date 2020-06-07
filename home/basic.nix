{
  imports = [
    ./generic
  ];
  setup = {
    source = ./basic.nix;
    shells.enableZsh = true;
  };
}
