#!/usr/bin/env bash

cd "$(dirname "$BASH_SOURCE")"

nix build -f "<nixpkgs/nixos>" config.system.build.isoImage -I nixos-config=./usb.nix
