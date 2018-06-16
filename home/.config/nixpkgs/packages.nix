{ pkgs }:
let
  git = (import ~/nixpkgs {});
in
  with pkgs; [
    chromium
    dropbox-cli
    liferea
    thunderbird

    git.cargo-edit
    git.cargo-tree
  ]
