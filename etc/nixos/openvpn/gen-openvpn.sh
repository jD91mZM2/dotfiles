#!/usr/bin/env bash

set -e

if [ -z "$1" ]; then
    echo "gen-openvpn.sh <directory> [default endpoint]"
    exit 1
fi

dir="$(realpath "$1")"
cd "$dir"

echo "{"
echo "  services.openvpn.servers = {"
for file in *.conf; do
    name="$(echo -n "$file" | sed -e 's/^\(.*\)\.conf$/\L\1/' -e 's/_/-/g')"
    autostart="false"
    if [ "$name" == "${2,,}" ]; then
        autostart="true"
    fi
    echo "    $name = { config = \"config $dir/$file\"; autoStart = $autostart; };"
done
echo "  };"
echo "}"
