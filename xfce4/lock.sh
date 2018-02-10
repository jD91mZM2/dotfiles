#!/bin/sh

# Undim screen
xrandr --output "$(xrandr | grep primary | cut -d ' ' -f 1)" --brightness 1

# Pause notifications
pkill dunst -USR1

xflock4

# Start notifications
pkill dunst -USR2
