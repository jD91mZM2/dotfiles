{ pkgs }:
let
  unstable = (import <nixos-unstable> {});
in
  with pkgs; [
    chromium
    dropbox-cli
    liferea
    thunderbird

    unstable.cargo-edit
    unstable.cargo-tree
  ]
