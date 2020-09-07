let
  flake = import <dotfiles>;
  mkNixosModule = flake.lib.system."${builtins.currentSystem}".mkNixosModule;
in
{
  network.description = "My personal VPS network";
  resources.sshKeyPairs.ssh-key = {};

  main = mkNixosModule (import ./main);
}
