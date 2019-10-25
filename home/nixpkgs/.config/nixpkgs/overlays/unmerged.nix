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
    powerline-rs = (
      for-commit
        "6abac910c0255daa5679f76fcde6e0d69401cb4d"
        "1lxld1y2qxlsvv6dgbf44c2b07h2yaq02fifgpjdcsvzwx518rv8"
    ).powerline-rs;
    yaru-theme = (
      for-commit
        "817e74524f5582de19e94bd348a91aa5466a6faa"
        "1h8fzvvipiidj1gzmdgb776l7wkxq5wj22niilhx01m43v8sn4iq"
    ).yaru-theme;
  };
}
