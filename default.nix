# From legacy Nix, import this to evaluate the flake.nix and get all flake outputs

(import (builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz") {
  src = ./.;
}).defaultNix
