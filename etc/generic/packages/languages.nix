{ config, pkgs, lib, ... }:

let
  cfg = config.setup.packages.languages;

  my-preferred-java-version = pkgs.openjdk8;
in
{
  options.setup.packages.languages = with lib; {
    c = mkEnableOption "C packages";
    elm = mkEnableOption "Elm packages";
    go = mkEnableOption "Go packages";
    haskell = mkEnableOption "Haskell packages";
    java = mkEnableOption "Java packages";
    latex = mkEnableOption "LaTeX packages";
    markdown = mkEnableOption "Markdown packages";
    python = mkEnableOption "Python packages";
    rust = mkEnableOption "Rust packages";
    wasm = mkEnableOption "WebAssembly packages";
  };

  config = {
    setup.home.modules = lib.singleton ({
      home.sessionVariables.JAVA_HOME = lib.optionalString cfg.java "${my-preferred-java-version.home}";

      home.packages = with pkgs;
        lib.optionals cfg.c [
          clangd
          cmake
          gcc
          gdb
          gnumake
        ]
        ++ lib.optionals cfg.elm [
          elmPackages.elm
          elmPackages.elm-format
        ]
        ++ lib.optionals cfg.go [
          go
          gopls
        ]
        ++ lib.optionals cfg.haskell [ stack ]
        ++ lib.optionals cfg.java [
          my-preferred-java-version
          jetbrains.idea-community
        ]
        ++ lib.optionals cfg.latex [
          poppler_utils
          texlive.combined.scheme-full
        ]
        ++ lib.optionals cfg.markdown [
          mdl
        ]
        ++ lib.optionals cfg.python [
          (python3.withPackages (p: with p; [
            # python-language-server - broken?
            tkinter
          ]))

          pypi2nix
        ]
        ++ lib.optionals cfg.rust [
          cargo-edit
          cargo-release
          rust-analyzer
          rustup

          crate2nix
        ]
        ++ lib.optionals cfg.wasm [
          wabt
        ];
    });
  };
}
