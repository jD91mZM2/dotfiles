self: super:

{
  nur = let
    nur = (import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") { pkgs = self; });
  in nur // {
    repos = nur.repos // {
      jd91mzm2 = import <dotfiles/nur-packages> { pkgs = self; };
    };
  };
}
