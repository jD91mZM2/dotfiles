let
  flake = import <dotfiles>;
  inherit (flake.lib."${builtins.currentSystem}") mkNixosModule;
in
{
  network.description = "My personal VPS network";
  resources.sshKeyPairs.ssh-key = { };

  main = mkNixosModule (import ./main);
}
