#!/usr/bin/env bash

cd "$(dirname "$BASH_SOURCE")"

nix build -f "<nixpkgs/nixos>" config.system.build.sdImage -I nixos-config=./rpi.nix
