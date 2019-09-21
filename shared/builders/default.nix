{ callPackage }:

{
  buildPypiPackage = callPackage ./pypi.nix {};
  buildRustPackage = callPackage ./rust.nix {};
}
