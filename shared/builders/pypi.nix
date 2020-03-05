{ callPackage, curl, lib, python36Packages, stdenv, pkgs }:

{ name, src }:

let
  python = callPackage (src + "/requirements.nix") {};
in python.mkDerivation {
  inherit name;
  src = pkgs.nur.repos.jd91mzm2.lib.sourceByNotRegex src [ ''^.*\.sqlite$'' ];

  buildInputs = builtins.attrValues python.packages;
  format = "other";
  installPhase = ''
    if [ -d src ]; then
      # Some of my projects have moved to having a src/ directory instead of just dumping all .py files in the root
      cd src
    fi

    mkdir $out
    cp -R . $out/src

    mkdir -p $out/bin
    cat > $out/bin/start <<EOF
      #!${stdenv.shell}
      if [ -f $out/src/__main__.py ]; then
        cd $out
        ${python.interpreter}/bin/python -m src
      else
        # Compatibility crux which I should fix some time
        ${python.interpreter}/bin/python $out/src/main.py
      fi
    EOF
    chmod +x $out/bin/start
  '';
}
