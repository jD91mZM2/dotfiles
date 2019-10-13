self: super:

let
  unstable = import <nixos-unstable> { overlays = []; };
  shared = self.callPackage <dotfiles/shared> {};

  rust = (self.rustChannelOf {
    channel = "nightly";
    date = "2019-10-02";
  }).rust;
  rustPlatform = self.makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };

  # So it turns out nativeBuildInputs will have a rust package left
  # which messes things up.
  withoutRust = pkgs: builtins.filter (p: !(builtins.elem (builtins.parseDrvName p.name).name ["cargo" "rustc"])) pkgs;

  rustSoftware = (
    { from ? null, name, src, hash, extra ? {} }:
    rustPlatform.buildRustPackage (builtins.foldl' (x: y: x // y) {} [
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
        src = shared.utils.cleanSource src;
        cargoSha256 = hash;
        doCheck = false;
      })

      extra
    ])
  );
in {
  local = {
    termplay = rustSoftware {
        from = unstable.termplay;
        name = "termplay";
        src = ~/Coding/Rust/termplay;
        hash = "0nr4xii09z6djdj9586b5mpncp7n3xlng65czz3g777ylwj0f7v2";
    };
    xidlehook = rustSoftware rec {
        from = unstable.xidlehook;
        name = "xidlehook";
        src = ~/Coding/Rust/xidlehook;
        hash = "0jdkcxvlw7s8pz1ka3d2w97356a2axvlwfgyh2dz7nmfzpjx64x0";
        extra = {
            buildInputs = with self; [ xorg.xlibsWrapper xorg.libXScrnSaver libpulseaudio ];
            nativeBuildInputs = (withoutRust from.nativeBuildInputs) ++ (with self; [ python3 ]);
            cargoBuildFlags = ["--bins"];
        };
    };
    powerline-rs = rustSoftware {
        from = unstable.powerline-rs;
        name = "powerline-rs";
        src = ~/Coding/Rust/powerline-rs;
        hash = "1sr9vbfk5bb3n0lv93y19in1clyvbj0w3p1gmp4sbw8lx84zwxhc";
    };
  };
}
