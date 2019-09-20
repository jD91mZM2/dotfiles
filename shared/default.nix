{ callPackage }:

{
  consts = import ./consts.nix;
  utils = callPackage ./utils.nix {};
}
