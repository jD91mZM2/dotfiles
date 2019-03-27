{ pkgs, lib }:
let
  unstable = import <nixos-unstable> {};
  excludeTarget = source: lib.cleanSource (lib.cleanSourceWith {
    filter = (path: _type: path != (toString (source + "/target")));
    src = source;
  });
  rust = pkgs.latest.rustChannels.stable;
  buildRustPackage = pkgs.rustPlatform.buildRustPackage.override {
    rust = {
      cargo = rust.cargo;
      rustc = rust.rust;
    };
  };

  rustSoftware = map
    ({ from ? null, name, src, hash, cargoBuildFlags ? null }: buildRustPackage (
      {
        name = "${name}-local";
        src = excludeTarget src;
        cargoSha256 = hash;
        doCheck = false;
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
        ) // (if cargoBuildFlags == null
        then {}
        else { inherit cargoBuildFlags; }
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
      hash = "1sy7q875gg6as98lp6m15x9b3lhdikm9326lmqcs5fv3hhzvdlvy";
      cargoBuildFlags = ["--bins"];
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
