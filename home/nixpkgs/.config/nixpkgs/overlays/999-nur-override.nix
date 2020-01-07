self: super:

{
  nur.jd91mzm2 = import (builtins.fetchTarball "https://gitlab.com/jD91mZM2/nur-packages/-/archive/master.tar.gz") { pkgs = self; };
}
