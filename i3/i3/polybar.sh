#!/bin/sh

for monitor in $(xrandr | grep "\bconnected\b" | grep -v "primary" | cut -d ' ' -f 1); do
    MONITOR="$monitor" polybar example & disown
done

MONITOR="$(xrandr | grep "primary" | cut -d ' ' -f 1)" polybar example & disown
# Primary last because traybar
