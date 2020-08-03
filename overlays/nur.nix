final: prev:

{
  nur = let
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") { pkgs = final; };
  in nur // {
    repos = nur.repos // {
      jd91mzm2 = import <dotfiles/nur-packages> { pkgs = final; };
    };
  };
}
