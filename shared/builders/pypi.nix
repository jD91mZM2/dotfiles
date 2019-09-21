{ callPackage, curl, lib, python36Packages, stdenv }:

{ name, src }:

let
  shared = callPackage <dotfiles/shared> {};

  python = callPackage (src + "/requirements.nix") {};
in python.mkDerivation {
  inherit name;
  src = shared.utils.cleanSource src;
  buildInputs = builtins.attrValues python.packages;
  format = "other";
  installPhase = ''
    mkdir -p $out/src
    cp *.py $out/src

    mkdir -p $out/bin
    cat > $out/bin/start <<EOF
      #!${stdenv.shell}
      ${python.interpreter}/bin/python $out/src/main.py
    EOF
    chmod +x $out/bin/start
  '';
}
