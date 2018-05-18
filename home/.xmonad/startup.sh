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
dropbox start &
liferea &
nm-applet &
st &
thunderbird &
xfce4-panel --disable-wm-check &
xfce4-power-manager &

chromium --app="https://chat.redox-os.org/" &
sleep 60 # please chromium just work
chromium --app="https://discordapp.com/channels/@me" &
