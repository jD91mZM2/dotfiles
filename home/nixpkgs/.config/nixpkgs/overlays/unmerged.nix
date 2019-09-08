self: super:

let
  for-commit = rev: sha256: (
    let
      code = self.fetchFromGitHub {
        owner = "jD91mZM2";
        repo = "nixpkgs";
        inherit rev sha256;
      };
    in
      import code { overlays = []; }
  );
in
{
  yaru-theme = (for-commit "b5af6bd2509ba4fe5e4eea83b190e81cc2720176" "12gkz1gggrxdp0gw6bfqc2fhcsqqvf3zn929llgdshyjb0pbmac8").yaru-theme;
}
