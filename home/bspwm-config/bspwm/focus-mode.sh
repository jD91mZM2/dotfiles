#!/usr/bin/env bash

if [ -e "/tmp/focus-mode" ]; then
    feh --bg-fill ~/Pictures/Backgrounds/background-focus.jpg --no-fehbg
    systemctl --user stop polybar

    # bspwm settings
    ~/.config/bspwm/setup-gap.sh 2 24 0

    # pause notifications
    pkill dunst -USR1

    rm /tmp/focus-mode
else
    feh --bg-fill ~/Pictures/Backgrounds/background.jpg --no-fehbg
    ~/.config/bspwm/monitor-setup.sh
    systemctl --user start polybar

    # bspwm settings
    ~/.config/bspwm/setup-gap.sh 1

    # resume notifications
    pkill dunst -USR2

    touch /tmp/focus-mode
fi

