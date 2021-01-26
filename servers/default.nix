let
  # Morph currently uses legacy Nix, so we build our system using a
  # compatibility layer
  makeModule = (import ../.).lib.makeModule "x86_64-linux";
in
{
  "main" = makeModule ./main;
}
