final: prev:

{
  clangd = (
    let
      clang = final.llvmPackages.clang-unwrapped;
    in
      final.runCommand "clangd-${final.stdenv.lib.getVersion clang}" {} ''
        mkdir -p "$out/bin"
        ln -s "${clang}/bin/clangd" "$out/bin/clangd"
      ''
  );
}
