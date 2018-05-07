#!/bin/sh

# Setup

xinput disable "$(xinput list | grep Touchpad | cut -d '=' -f 2 | cut -d $'\t' -f 1)"
xsetroot -cursor_name left_ptr
~/.fehbg

# Background

systemctl start --user compton
udiskie &
~/.dotfiles/lock-timer.sh slock &

# Interactive

"$DEJA_DUP_MONITOR" &
Discord &
dropbox start &
konsole &
liferea &
chromium --app="https://chat.redox-os.org/" &
nm-applet &
thunderbird &
xfce4-panel --disable-wm-check &
