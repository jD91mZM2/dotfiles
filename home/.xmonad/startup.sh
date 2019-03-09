#!/bin/sh

# Setup

xsetroot -cursor_name left_ptr
~/.fehbg

# Background

systemctl start --user compton
udiskie &
~/dotfiles/lock-timer.sh slock &

# Interactive

liferea &
nm-applet &
st &
thunderbird &
xfce4-panel --disable-wm-check &
xfce4-power-manager &

chromium --app="https://chat.redox-os.org/" &
sleep 10 # a few programs don't handle stress well
chromium --app="https://discordapp.com/channels/@me" &

st tmux new -s weechat -- mosh scaleway -- tmux attach -t weechat &
