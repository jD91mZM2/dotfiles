#!/bin/sh

cd "$(dirname "$0")"

action="${1?:./dmenu-confirm.sh <action> <command>}"
result="$(echo "Cancel"$'\n'"$action" | ./dmenu.sh -l 0)"

if [ "$result" != "$action" ]; then
    exit 1
fi
