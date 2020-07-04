{ config, pkgs, self, shared, nur, sharedBase, ... }:

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
    config = { pkgs, ... }: {
      # Somewhat hacky way to set extraArgs for the containers and therefore
      # make sharedBase work.
      _module.args = {
        inherit self shared;
      };

      imports = [
        nur.nixosModules.programs
        sharedBase
      ];

      environment.systemPackages = with pkgs; [
        nodejs
      ];
    };
    # path = mkNixosConfig ({ pkgs, ... }: {
    #   environment.systemPackages = with pkgs; [
    #     nodejs
    #   ];
    # });
  };
}
