{
  network.description = "My personal VPS network";
  resources.sshKeyPairs.ssh-key = {};

  main = import ./main;
  rpi  = import ./rpi;
}
