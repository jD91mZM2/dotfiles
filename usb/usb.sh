#!/bin/sh

cd "$(basedir "$1")"
nix build -f "<nixpkgs/nixos>" config.system.build.isoImage -I nixos-config=usb.nix
