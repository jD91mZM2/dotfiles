{ config, pkgs, ... }:

let
  shared = import <dotfiles/shared> {};
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
    config = { pkgs, ... }: {
      imports = [
        <dotfiles/shared/base.nix>
      ];

      environment.systemPackages = with pkgs; [
        nodejs
      ];
    };
  };

  # Because nix-flakes are unstable
  containers.nix-flakes = {
    bindMounts = {
      "/home" = { isReadOnly = false; };
    };
    config = { pkgs, ... }: {
      imports = [
        <dotfiles/shared/base.nix>
      ];

      nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };
    };
  };
}
