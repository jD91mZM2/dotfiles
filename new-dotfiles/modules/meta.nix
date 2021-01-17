{ lib, ... }:

{
  nix = {
    binaryCaches = [
      # Input Output HK - Hydra cache for haskell.nix
      "https://hydra.iohk.io"

      # Cachix Caches
      "https://iohk.cachix.org"
      "https://nix-community.cachix.org"
      "https://jd91mzm2.cachix.org"
    ];
    binaryCachePublicKeys = [
      # Input Output HK - Hydra cache for haskell.nix
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="

      # Cachix Caches
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "jd91mzm2.cachix.org-1:Ozz1Jhba7XCQidfZuptUFWy0tpnqtg6v1gJpuLIkyyY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    registry.nixpkgs = {
      from = {
        type = "indirect";
        id = "nixpkgs";
      };
      to = lib.mkForce {
        type = "github";
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "nixos-unstable";
      };
    };

    # Keep my harddrive relatively small
    gc = {
      automatic = true;
      dates = "Mon *-*-* 17:00";
      options = "--delete-older-than 10d";
    };
    optimise = {
      automatic = true;
      dates = [ "17:00" ];
    };
  };
}
