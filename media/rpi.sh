#!/usr/bin/env bash

cd "$(dirname "$BASH_SOURCE")"

nix build -f "<nixpkgs/nixos>" --arg crossSystem "(import <nixpkgs/lib>).systems.examples.raspberryPi" config.system.build.sdImage -I nixos-config=./rpi.nix
