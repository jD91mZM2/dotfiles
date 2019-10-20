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
  yaru-theme = (
    for-commit
      "817e74524f5582de19e94bd348a91aa5466a6faa"
      "1h8fzvvipiidj1gzmdgb776l7wkxq5wj22niilhx01m43v8sn4iq"
  ).yaru-theme;
}
