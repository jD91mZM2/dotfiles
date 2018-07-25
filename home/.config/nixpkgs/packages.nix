{ pkgs }:
let
  unstable = import <nixos-unstable> {};
  excludeTarget = source: builtins.filterSource (path: _type: path != (toString (source + "/target"))) source;
in
  with pkgs; [
    chromium
    dropbox-cli
    liferea
    thunderbird

    unstable.cargo-edit
    unstable.cargo-tree

    # My own software
    (unstable.rustPlatform.buildRustPackage (rec {
      name = "termplay-local";
      src = excludeTarget ~/Coding/Rust/termplay;
      cargoSha256 = "1sml3wpwkcc60vcm8gaw9d04a2ndzzp5fbsmkanf5sixmzdms0mx";
      inherit (unstable.termplay) cargoBuildFlags buildInputs nativeBuildInputs postInstall meta;
    }))
    (unstable.rustPlatform.buildRustPackage (rec {
      name = "xidlehook-local";
      src = excludeTarget ~/Coding/Rust/xidlehook;
      cargoSha256 = "1mrg59flmmqg5wwi2l8lw6p1xpgdw597fdfsmpn8b126rgzqmjl8";
      inherit (unstable.xidlehook) cargoBuildFlags buildInputs nativeBuildInputs postFixup meta;
    }))
  ]
