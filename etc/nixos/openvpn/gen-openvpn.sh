#!/usr/bin/env bash

set -e

usage="gen-openvpn.sh <directory udp> <directory tcp> [default endpoint]"
: "${1:?$usage}"
: "${2:?$usage}"

echo "{"
echo "  services.openvpn.servers = {"

genDir() {
    : "${1:?}"
    dir="$(realpath "$1")"
    pushd "$dir" > /dev/null

    for file in *.conf; do
        name="$(echo -n "$file" | sed -e 's/^\(.*\)\.conf$/\L\1/' -e 's/_/-/g')$3"
        autostart="false"
        if [ "$name" == "${2,,}" ]; then
            autostart="true"
        fi
        echo "    $name = { config = \"config $dir/$file\"; autoStart = $autostart; };"
    done
    popd > /dev/null
}
genDir "$1" "$3"
genDir "$2" "$3" "-tcp"

echo "  };"
echo "}"
