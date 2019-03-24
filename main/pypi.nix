name: src:
{ pkgs, curl, python36Packages }:

let
  python = import (src + "/requirements.nix") { inherit pkgs; };
in python.mkDerivation rec {
  inherit name src;
  buildInputs = builtins.attrValues python.packages;
  format = "other";
  installPhase = ''
    mkdir -p $out/src
    cp *.py $out/src

    mkdir -p $out/bin
    cat > $out/bin/start <<EOF
      #!/bin/sh
      mkdir "/home/hapPy/${name}" || true
      cd "/home/hapPy/${name}"
      ${python.interpreter}/bin/python $out/src/main.py
    EOF
    chmod +x $out/bin/start
  '';
}
