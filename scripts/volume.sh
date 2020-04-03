#!/usr/bin/env bash

set -e

# Note: This assumes I only have one sink. Fix this if needed.
percent="$(pactl list sinks | sed 's/.*\s\+\([0-9]\+\)%.*/\1/g; t q; d; :q q')"

case "${1:?}" in
    up)
        new="$((percent * 125 / 100))" # * 1.25

        if [ "$new" == "0" ]; then
            new=4
        elif [ "$new" -gt "100" ]; then
            new=97
        fi
        ;;
    down)
        if [ "$percent" -lt "5" ]; then
            new=0
        else
            new="$(((percent + 1) * 100 / 125))" # / 1.25
        fi
        ;;
    *)
        echo "./volume.sh [up|down]"
        exit 1
esac

pactl set-sink-volume "@DEFAULT_SINK@" "$new%"
