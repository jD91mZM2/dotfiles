# This sucks, but so does Rust's Nix support. Don't blame me.

self: super:

let
  rust = self.latest.rustChannels.beta.rust;
  link = { name, dir ? name, args ? [], deps ? [] }:
    let
      path = "~/Coding/Rust/${self.lib.escapeShellArg dir}";
    in self.writeShellScriptBin name ''
      nix-shell ${path}/shell.nix --run "$(cat <<EOF
        export RUSTC="${rust}/bin/rustc"
        "${rust}/bin/cargo" run -q \
          --manifest-path ${path}/Cargo.toml \
          ${self.lib.escapeShellArgs args}
      EOF
      )"
    '';
in {
  local = {
    termplay = link { name = "termplay"; args = ["--features" "bin"]; };
    xidlehook = link { name = "xidlehook"; };
    xidlehook-client = link { name = "xidlehook-client"; dir = "xidlehook"; args = ["--bin" "xidlehook-client"]; };
  };
}
