#!/bin/sh

dirname="$(dirname "$0")"

cd
# Setup

xsetroot -cursor_name left_ptr

# Always running

nm-applet &

# Applications

thunderbird &
xfce4-power-manager &

st tmux &
sleep 1 # wait for terminal to statup

rm /tmp/nvimsocket
st -n neovim e &

st -n weechat weechat &

chromium --app="https://chat.redox-os.org/" &
sleep 10 # chrome doesn't handle stress well apparently
chromium --app="https://discordapp.com/channels/@me" &
