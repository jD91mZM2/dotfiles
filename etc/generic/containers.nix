{ config, ... }:

{
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
