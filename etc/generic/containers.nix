{ config, pkgs, self, shared, inputs, system, ... }:

let
  inherit (self.lib."${system}") mkNixosModule;
in
{
  # Because most npm packages are horribly unsafe, embedding native libraries
  # compiled by randos and downloading binaries all over the place.
  containers.nodejs = {
    bindMounts = {
      "${shared.consts.home}" = {
        hostPath = "${shared.consts.home}/Coding/JavaShit";
        isReadOnly = false;
      };
    };
    config = mkNixosModule ({ pkgs, ... }: {
      environment.systemPackages = with pkgs; [
        nodejs
      ];

      programs.npm.enable = true;
      environment.variables.PATH = "$PATH:$HOME/.npm/bin";
    });
  };
}
