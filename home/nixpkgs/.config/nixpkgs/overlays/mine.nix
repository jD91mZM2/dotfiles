self: super:

let
  unstable = import <nixos-unstable> { overlays = []; };
  excludeTarget = source: self.lib.cleanSource (self.lib.cleanSourceWith {
    filter = (path: _type: path != (toString (source + "/target")));
    src = source;
  });
  rust = self.latest.rustChannels.stable;
  buildRustPackage = self.rustPlatform.buildRustPackage.override {
    cargo = rust.cargo;
    rustc = rust.rust;
  };

  rustSoftware = (
    { from ? null, name, src, hash, cargoBuildFlags ? null }:
    buildRustPackage (builtins.foldl' (x: y: x // y) {} [
      (if from == null
        then {}

        # Inherit these attribute, *if* they exist
        else (builtins.intersectAttrs {
          buildInputs = null;
          cargoBuildFlags = null;
          meta = null;
          nativeBuildInputs = null;
          postFixup = null;
          #postInstall = null;
        } from)
      )

      ({
        name = "${name}-local";
        src = excludeTarget src;
        cargoSha256 = hash;
        doCheck = false;
      })
    ])
  );
in {
  termplay = rustSoftware {
    from = unstable.termplay;
    name = "termplay";
    src = ~/Coding/Rust/termplay;
    hash = "0nr4xii09z6djdj9586b5mpncp7n3xlng65czz3g777ylwj0f7v2";
  };
  xidlehook = rustSoftware {
    from = unstable.xidlehook;
    name = "xidlehook";
    src = ~/Coding/Rust/xidlehook;
    hash = "0fl3r4y2wp514hp3qpq6h0rj95jv25n86w0mwdia7wvjaw0157wg";
    cargoBuildFlags = ["--bins"];
  };
  powerline-rs = rustSoftware {
    from = unstable.powerline-rs;
    name = "powerline-rs";
    src = ~/Coding/Rust/powerline-rs;
    hash = "1sr9vbfk5bb3n0lv93y19in1clyvbj0w3p1gmp4sbw8lx84zwxhc";
  };
}
