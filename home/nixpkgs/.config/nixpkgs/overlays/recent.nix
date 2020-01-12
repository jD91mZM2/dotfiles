self: super:

let
  for-rev = branch: (
    import (builtins.fetchTarball "https://github.com/jD91mZM2/nixpkgs/archive/${branch}.tar.gz") { overlays = []; }
  );
in {
  unmerged = {
    git-subcopy = (for-rev "git-subcopy").git-subcopy;
    scaff       = (for-rev "scaff").scaff;
  };
  master = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { overlays = []; };
}
