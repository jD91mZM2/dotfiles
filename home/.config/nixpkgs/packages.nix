{ pkgs }:
let
  unstable = import <nixos-unstable> {};
  excludeTarget = source: builtins.filterSource (path: _type: path != (toString (source + "/target"))) source;
in
  with pkgs; [
    # My own software
    (unstable.rustPlatform.buildRustPackage (rec {
      name = "termplay-local";
      src = excludeTarget ~/Coding/Rust/termplay;
      cargoSha256 = "0nr4xii09z6djdj9586b5mpncp7n3xlng65czz3g777ylwj0f7v2";
      inherit (unstable.termplay) cargoBuildFlags buildInputs nativeBuildInputs postInstall meta;
    }))
    (unstable.rustPlatform.buildRustPackage (rec {
      name = "xidlehook-local";
      src = excludeTarget ~/Coding/Rust/xidlehook;
      cargoSha256 = "1mrg59flmmqg5wwi2l8lw6p1xpgdw597fdfsmpn8b126rgzqmjl8";
      inherit (unstable.xidlehook) cargoBuildFlags buildInputs nativeBuildInputs postFixup meta;
    }))
  ]
