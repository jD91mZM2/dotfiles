{
  pkgs ? import <nixpkgs> {},
  callPackage ? pkgs.callPackage,
  ...
}:

{
  consts   = import ./consts.nix;
  builders = callPackage ./builders {};
  theme    = import ./dracula.nix;
  modules  = import ./modules;
}
