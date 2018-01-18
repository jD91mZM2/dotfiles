#!/bin/sh

# Pause notifications
pkill dunst -USR1

i3lock -knc "#111111" --timecolor "#FFFFFFFF" --textcolor "#FFFFFFFF" --datecolor "#FFFFFFFF"

# Start notifications
pkill dunst -USR2
