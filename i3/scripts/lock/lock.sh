#!/bin/sh

# Pause notifications
pkill dunst -USR1

i3lock \
    --clock \
    --nofork \
    --color     "#111111" \
    --datecolor "#FFFFFFFF" \
    --textcolor "#FFFFFFFF" \
    --timecolor "#FFFFFFFF" \
    --timestr   "%I:%M:%S"

# Start notifications
pkill dunst -USR2
