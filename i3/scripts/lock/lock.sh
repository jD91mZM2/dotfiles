#!/bin/sh

# Undim screen
xrandr --output "$(xrandr | grep primary | cut -d ' ' -f 1)" --brightness 1

# Pause notifications
pkill dunst -USR1

i3lock \
    --clock \
    --nofork \
    --timesize 24 \
    --color     "#111111" \
    --datecolor "#FFFFFFFF" \
    --textcolor "#FFFFFFFF" \
    --timecolor "#FFFFFFFF" \
    --timestr   "%I:%M:%S%p"

# Start notifications
pkill dunst -USR2
