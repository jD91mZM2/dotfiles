# This file builds rust at runtime, simply to escape nix's sandboxing that
# prevents cargo from being cached. Using carnix would be better, if only it
# worked as reliably as cargo itself.

{ callPackage, binutils-unwrapped, lib, rsync, stdenv }:

{ name, src, buildInputs ? [], wrapperHook ? "" }:

let
  utils = callPackage ../utils.nix {};
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  moz_pkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustChan = (moz_pkgs.rustChannelOf {
    date = "2019-09-11";
    channel = "nightly";
  });
in stdenv.mkDerivation {
  inherit name buildInputs;
  src = utils.cleanSource src;
  doConfigure = false;
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/src
    cp -r * $out/src
    mkdir -p $out/bin
    cat > $out/bin/start <<EOF
      #!${stdenv.shell}
      set -e

      ${rsync}/bin/rsync -rE --del --exclude target $out/src .

      export PATH="${lib.makeBinPath (buildInputs ++ [rustChan.rust stdenv.cc binutils-unwrapped])}:\$PATH"
      export LIBRARY_PATH="${lib.makeLibraryPath buildInputs}"
      # below vars are expanded at build time:
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH"

      ${wrapperHook}

      cargo run --release --manifest-path src/Cargo.toml
    EOF
    chmod +x $out/bin/start
  '';
}
