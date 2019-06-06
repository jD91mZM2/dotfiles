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

    autostart_done=0

    last="invalid-thing"
    count=0

    find . -maxdepth 1 -print0 | shuf -z | while read -r -d "" file; do
        file="$(echo -n "$file" | sed "s|^\./||")"
        # There are so many servers nix would take forever to build them all!! :)
        if [[ "$file" == "$last"* ]]; then
            [ "$count" -gt 4 ] && continue
            count=$((count + 1))
        else
            last="$(echo -n "$file" | cut -c -2)"
            count=1
        fi

        name="$(echo -n "$file" | sed -e 's/^\([a-zA-Z0-9-]\+\).*\.ovpn$/\L\1/')$3"
        autostart="false"
        if [ -n "$2" ] && [[ "$name" == "${2,,}"* ]] && [ "$autostart_done" == 0 ]; then
            autostart="true"
            autostart_done=1
        fi
        echo "    $name = { config = \"config $dir/$file\"; autoStart = $autostart; };"
    done
    popd > /dev/null
}
genDir "$1" "$3"
genDir "$2" "" "-tcp"

echo "  };"
echo "}"
