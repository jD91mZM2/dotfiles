self: super:

let
  for-rev = branch: (
    import (builtins.fetchTarball "https://github.com/jD91mZM2/nixpkgs/archive/${branch}.tar.gz") { overlays = []; }
  );
in {
  # xidlehook = import (builtins.fetchTarball "https://gitlab.com/jD91mZM2/xidlehook/-/archive/master.tar.gz") { pkgsFn = import <nixpkgs>; };
  unmerged = {
    scaff = (for-rev "scaff").scaff;
  };
  master = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { overlays = []; };
}
