self: super:

{
  clangd = (
    let
      clang = self.llvmPackages.clang-unwrapped;
    in
      self.runCommand "clangd-${self.stdenv.lib.getVersion clang}" {} ''
        mkdir -p "$out/bin"
        ln -s "${clang}/bin/clangd" "$out/bin/clangd"
      ''
  );
}
