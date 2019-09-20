{
  pkgs ? import <nixpkgs> {},
  callPackage ? pkgs.callPackage,
  ...
}:

{
  consts = import ./consts.nix;
  utils = callPackage ./utils.nix {};
}
