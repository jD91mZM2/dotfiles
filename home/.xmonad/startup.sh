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
chromium --app="https://chat.redox-os.org/" &
dropbox start &
konsole &
liferea &
nm-applet &
thunderbird &
xfce4-panel --disable-wm-check &
xfce4-power-manager &
