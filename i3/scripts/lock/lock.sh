#!/bin/sh

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
