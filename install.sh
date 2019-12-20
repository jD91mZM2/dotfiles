#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git vim

set -e

# Art of "Install NixOS", but some characters are escaped wih `\`
cat <<EOF
 ___           _        _ _   _   _ _       ___  ____
|_ _|_ __  ___| |_ __ _| | | | \\ | (_)_  __/ _ \\/ ___|
 | || '_ \\/ __| __/ _\` | | | |  \\| | \\ \\/ / | | \\___ \\
 | || | | \\__ \\ || (_| | | | | |\\  | |>  <| |_| |___) |
|___|_| |_|___/\\__\\__,_|_|_| |_| \\_|_/_/\\_\\\\___/|____/
EOF

if [[ "$PWD" != "/mnt/"* ]]; then
    echo "Warning: You're not inside /mnt, any modifications will probably get lost after a reboot."
fi

echo "Cloning dotfiles..."
git clone https://gitlab.com/jD91mZM2/dotfiles || true

pushd dotfiles
pushd etc

read -p "What should this setup be called? " -r name

if [ ! -e "$name" ]; then
    cp -r template "$name"
    sed -i \
"s|@@NETWORK_ID@@|$(head -c8 /etc/machine-id)|g" \
"s|@@DEVICE_NAME@@|$name|g" \
"$name/configuration.nix"
fi

echo "Generating hardware config..."
pushd "$name"
nixos-generate-config --root /mnt --show-hardware-config > hardware-configuration.nix

echo "Ready to modify configuration? Vim will launch."
read -p "[Press any character to continue]" -rn1
vim -u NONE configuration.nix

popd
popd

echo "Building system closure..."

system="$(nix-build \
-I nixos-config="$PWD/etc/$name/configuration.nix" \
-I dotfiles="$PWD" \
"<nixpkgs/nixos>" -A system --no-out-link)"

echo "Ready to install, when you are!"
read -rn1 -p "[Press any character to continue]"

sudo nixos-install --system "$system"
