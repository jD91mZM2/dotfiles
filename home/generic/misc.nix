{ pkgs, config, ... }:
let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  programs.scaff = {
    enable = true;
    imports = let
      baseURL = "https://gitlab.com/jD91mZM2/scaff-repo/-/jobs/512194133/artifacts/raw/build";
    in {
      build-rust-package = builtins.fetchurl "${baseURL}/build-rust-package.tar.gz";
      cachix             = builtins.fetchurl "${baseURL}/cachix.tar.gz";
      crate2nix          = builtins.fetchurl "${baseURL}/crate2nix.tar.gz";
      editorconfig       = builtins.fetchurl "${baseURL}/editorconfig.tar.gz";
      license-mit        = builtins.fetchurl "${baseURL}/license-mit.tar.gz";
      naersk             = builtins.fetchurl "${baseURL}/naersk.tar.gz";
      nix-shell          = builtins.fetchurl "${baseURL}/nix-shell.tar.gz";
      nixpkgs-rust       = builtins.fetchurl "${baseURL}/nixpkgs-rust.tar.gz";
      readme             = builtins.fetchurl "${baseURL}/readme.tar.gz";
      rustfmt            = builtins.fetchurl "${baseURL}/rustfmt.tar.gz";
    };
  };

  programs.mpv = {
    enable = true;
    config = {
      keep-open = true;
    };
  };

  home.file.".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink ./misc/tmux.conf;
  home.file.".editorconfig".source = config.lib.file.mkOutOfStoreSymlink ./misc/default.editorconfig;
  home.file.".gdbinit".source = config.lib.file.mkOutOfStoreSymlink ./misc/gdbinit;

  home.file.".stack/config.yaml".text =
    builtins.toJSON { # YAML is a JSON superset
      templates.params = {
        "author-name" = shared.consts.name;
        "author-email" = shared.consts.email;
        "github-username" = shared.consts.name;
      };
    };
}
