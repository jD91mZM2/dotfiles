#!/bin/sh

fmt="%-15s\n"

hidden=0
short=0
hide() {
    hidden="$((hidden ^ 1))"
    printf "$fmt" ""
}
shorten() {
    printf "$fmt" ""
}
cleanup() {
    kill $(jobs -p) 2> /dev/null
}

trap "hide; cleanup" USR1
trap "shorten; cleanup" USR2
trap "cleanup; exit" INT TERM

while true; do
    if [ "$hidden" = "0" ]; then
        printf "$fmt" "$(curl -s https://api.ipify.org)"
        sleep 60 & wait # Handles signals
    else
        printf "$fmt" "<ip goes here>"
        sleep inf & wait
    fi
done
