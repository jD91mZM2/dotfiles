#!/bin/sh

# Pause notifications
pkill dunst -USR1

i3lock -c "#111111"

# Start notifications
pkill dunst -USR2
