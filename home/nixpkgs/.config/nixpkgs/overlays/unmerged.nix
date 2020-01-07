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
in {
  unmerged = {
    git-subcopy  = (for-commit "6c53721d83933bed69e571bdf8f4daced6a9664e" "08kdr8mg6sz3dm8yj7svjr83k19r4zya7ml95lmn96h8y9lby1ba").git-subcopy;
    powerline-rs = (for-commit "d61427d6c75f46ce2d95412c776de02a6676c377" "19jlri3sx0r52hi3srzz047hdwjfbnj6qp1cyh143p22chifvvar").powerline-rs;
    scaff        = (for-commit "209b02af202f2affce0071266910ce97ead00365" "0rg7ih2my3dcsrc5xdmiq00f4zvyvvfg752zz2qw3vncqsszza3q").scaff;
    termplay     = (for-commit "63b5137662ef115b96d508abeef6cad42014d906" "1qgqvi4xg5vv7ly1qmnw3cplybrscva1zlqw7bkaxxy12vjjna73").termplay;
    yaru-theme   = (for-commit "817e74524f5582de19e94bd348a91aa5466a6faa" "1h8fzvvipiidj1gzmdgb776l7wkxq5wj22niilhx01m43v8sn4iq").yaru-theme;
  };
}
