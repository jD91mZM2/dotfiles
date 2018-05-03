{ stdenv, pkgs, lib, fetchFromGitHub, ... }:

stdenv.mkDerivation rec {
  name = "z-${version}";
  version = "1.11";

  src = fetchFromGitHub {
    owner = "rupa";
    repo = "z";
    rev = "v${version}";
    sha256 = "13zbgkj6y0qhvn5jpkrqbd4jjxjr789k228iwma5hjfh1nx7ghyb";
  };

  buildPhase = " "; # do nothing
  installPhase = ''
    install -Dm 644 z.sh $out/share/z.sh
    gzip z.1
    install -Dm 644 z.1.gz $out/share/man/man1/z.1.gz
  '';

  meta = {
    description = "Tracks your most used directories, based on 'frecency'.";
    license = lib.licenses.wtfpl;
  };
}
