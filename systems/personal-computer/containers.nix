{ config, self, system, ... }:

let
  home = "/home/${config.globals.userName}";
in
{
  # Because most npm packages are horribly unsafe, embedding native libraries
  # compiled by randos and downloading binaries all over the place.
  containers.nodejs = {
    bindMounts = {
      "${home}" = {
        hostPath = "${home}/Coding/JavaShit";
        isReadOnly = false;
      };
    };
    config = self.lib.makeModule system ({ pkgs, lib, ... }: {
      imports = [
        ../../templates/container.nix
      ];

      environment.systemPackages = with pkgs; [
        # JavaScript sucks
        nodejs

        # At least TypeScript makes it somewhat bearable
        nodePackages.typescript
      ];

      # This installs local packages to $HOME/.npm
      programs.npm.enable = true;
      # Add it to the $PATH
      environment.variables.PATH = "$PATH:$HOME/.npm/bin";
    });
  };
}
