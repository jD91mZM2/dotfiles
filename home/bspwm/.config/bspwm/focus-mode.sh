#!/usr/bin/env bash

if [ -e "/tmp/focus-mode" ]; then
    feh --bg-fill ~/Pictures/Backgrounds/background-focus.jpg --no-fehbg
    pkill polybar

    # bspwm settings
    bspc config window_gap 24
    bspc config top_padding 0

    # pause notifications
    pkill dunst -USR1

    rm /tmp/focus-mode
else
    feh --bg-fill ~/Pictures/Backgrounds/background.jpg --no-fehbg
    ~/.config/bspwm/monitor-setup.sh

    # bspwm settings
    bspc config window_gap 12
    bspc config top_padding 30 # obtained from `xprop | grep _NET_WM_STRUT`

    # resume notifications
    pkill dunst -USR2

    touch /tmp/focus-mode
fi

