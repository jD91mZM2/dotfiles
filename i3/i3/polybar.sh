#!/bin/sh

killall polybar

# Primary first because system tray
MONITOR="$(xrandr | grep "\bprimary\b" | cut -d ' ' -f 1)" polybar example & disown
sleep 2

for monitor in $(xrandr | grep "\bconnected\b" | grep -v "\bprimary\b" | cut -d ' ' -f 1); do
    MONITOR="$monitor" polybar example & disown
done
