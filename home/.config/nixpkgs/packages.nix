{ pkgs }:
let
  unstable = import <nixos-unstable> {};
  excludeTarget = source: builtins.filterSource (path: _type: path != (toString (source + "/target"))) source;
  stable = pkgs.latest.rustChannels.stable;
  buildRustPackage = pkgs.rustPlatform.buildRustPackage.override {
    rust = stable;
  };
  # See https://github.com/NixOS/nixpkgs/issues/25863#issuecomment-302633494
  RUSTFLAGS="-L ${stable.rust}/lib/rustlib/x86_64-unknown-linux-gnu/lib/";
in
  with pkgs; [
    # My own software
    (buildRustPackage (rec {
      name = "termplay-local";
      src = excludeTarget ~/Coding/Rust/termplay;
      cargoSha256 = "0nr4xii09z6djdj9586b5mpncp7n3xlng65czz3g777ylwj0f7v2";
      doCheck = false;
      inherit RUSTFLAGS;
      inherit (unstable.termplay) cargoBuildFlags buildInputs nativeBuildInputs postInstall meta;
    }))
    (buildRustPackage (rec {
      name = "xidlehook-local";
      src = excludeTarget ~/Coding/Rust/xidlehook;
      cargoSha256 = "1pdhbqnkgwp2v5zyin8z8049aq8c3kfk04v9wsbz8qla34rgi99s";
      inherit RUSTFLAGS;
      inherit (unstable.xidlehook) cargoBuildFlags buildInputs nativeBuildInputs postFixup meta;
    }))
  ]
