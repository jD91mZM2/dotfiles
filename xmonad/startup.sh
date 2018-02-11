#!/bin/sh

# Background

compton -b
udiskie &
~/.dotfiles/lock-timer.sh slock &

~/.screenlayout/default.sh
xsetroot -cursor_name left_ptr
xinput disable "$(xinput list | grep Touchpad | cut -d '=' -f 2 | cut -d $'\t' -f 1)"
feh --bg-fill /usr/share/archlinux/wallpaper/archlinux-firestarter.jpg
# Note: feh has to be after screenlayout

# Interactive

/usr/lib/deja-dup/deja-dup-monitor &
discord &
dropbox &
evolution &
konsole &
mattermost-desktop &
xfce4-power-manager &
