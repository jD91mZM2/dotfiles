#!/usr/bin/env bash

# Script for building the initial generation without all the parameters set

requested_dotfiles="$(nix eval -f shared/default.nix consts.dotfiles --raw)"
actual_dotfiles="$(dirname $(dirname $(realpath "${BASH_SOURCE[0]}")))"

if [ "$requested_dotfiles" != "$actual_dotfiles" ]; then
    echo "Warning: Nix-configured path \"$requested_dotfiles\" does not match current path \"$actual_dotfiles\""
fi

dotfiles="$requested_dotfiles"
config="$dotfiles/etc/nixos/configuration.nix"

cat <<EOF
 ____              _       _
| __ )  ___   ___ | |_ ___| |_ _ __ __ _ _ __
|  _ \ / _ \ / _ \| __/ __| __| '__/ _\` | '_ \\
| |_) | (_) | (_) | |_\__ \ |_| | | (_| | |_) |
|____/ \___/ \___/ \__|___/\__|_|  \__,_| .__/
                                        |_|

Dotfiles: "$dotfiles"
Config: "$config"

EOF

while true; do
    read -n1 -r -p "Does this look good? [Y/n] " input

    if [ -n "$input" ]; then
        echo
    fi

    if [ "${input,,}" == "y" ] || [ -z "$input" ]; then
        break
    elif [ "${input,,}" == "n" ]; then
        exit 1
    fi
done

readarray -td : sections < <(echo "$NIX_PATH")

new=()

for section in "${sections[@]}"; do
    if [[ "$section" == "dotfiles="* ]] || [[ "$section" == "nixos-config="* ]]; then
        continue
    fi
    new+=("$section")
done

new=("dotfiles=$dotfiles" "nixos-config=$config" "${new[@]}")

echo

for section in "${new[@]}"; do
    echo "NIX_PATH: $section"
done

export NIX_PATH="$(IFS=:; echo "${new[*]}")"

if [ -n "$SHELL" ]; then
    exec "$SHELL"
else
    exec bash
fi
