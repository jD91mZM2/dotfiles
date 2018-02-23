#!/bin/sh

# Setup

~/.screenlayout/default.sh
xsetroot -cursor_name left_ptr
xinput disable "$(xinput list | grep Touchpad | cut -d '=' -f 2 | cut -d $'\t' -f 1)"
feh --bg-fill /usr/share/archlinux/wallpaper/archlinux-firestarter.jpg
# Note: feh has to be after screenlayout

# Background

compton -b
udiskie &
~/.dotfiles/lock-timer.sh slock &

# Interactive

/usr/lib/deja-dup/deja-dup-monitor &
discord &
dropbox &
evolution &
konsole &
mattermost-desktop &
nm-applet &
xfce4-panel &
xfce4-power-manager &
