#!/bin/sh

cd "$(dirname "$1")"
nix build -f "<nixpkgs/nixos>" config.system.build.isoImage -I nixos-config=usb.nix
