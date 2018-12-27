{ pkgs, lib }:
let
  unstable = import <nixos-unstable> {};
  excludeTarget = source: builtins.filterSource (path: _type: path != (toString (source + "/target"))) source;
  rust = pkgs.latest.rustChannels.stable;
  buildRustPackage = pkgs.rustPlatform.buildRustPackage.override {
    inherit rust;
  };
  # See https://github.com/NixOS/nixpkgs/issues/25863#issuecomment-302633494
  RUSTFLAGS="-L ${rust.rust}/lib/rustlib/x86_64-unknown-linux-gnu/lib/";

  rustSoftware = map
    ({ from ? null, name, src, hash }: buildRustPackage (
      {
        name = "${name}-local";
        src = excludeTarget src;
        cargoSha256 = hash;
        doCheck = false;
        inherit RUSTFLAGS;
      } // (if from == null
        then {}
        else
          # Inherit these attribute, *if* they exist
          (builtins.intersectAttrs {
            buildInputs = null;
            cargoBuildFlags = null;
            meta = null;
            nativeBuildInputs = null;
            postFixup = null;
            #postInstall = null;
          } from)
        )
    )) [
    {
      from = unstable.termplay;
      name = "termplay";
      src = ~/Coding/Rust/termplay;
      hash = "0nr4xii09z6djdj9586b5mpncp7n3xlng65czz3g777ylwj0f7v2";
    }
    {
      from = unstable.xidlehook;
      name = "xidlehook";
      src = ~/Coding/Rust/xidlehook;
      hash = "1pdhbqnkgwp2v5zyin8z8049aq8c3kfk04v9wsbz8qla34rgi99s";
    }
    {
      from = unstable.powerline-rs;
      name = "powerline-rs";
      src = ~/Coding/Rust/powerline-rs;
      hash = "1sr9vbfk5bb3n0lv93y19in1clyvbj0w3p1gmp4sbw8lx84zwxhc";
    }
  ];
in
  with pkgs; rustSoftware
