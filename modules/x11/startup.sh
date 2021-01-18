#!/bin/sh

cd

# Applications

## Start terminal

st &

## Start editor

rm /tmp/nvimsocket
st -n neovim e &

## Start chats

st -n weechat weechat &

thunderbird &

chromium --app="https://chat.redox-os.org/" &
sleep 10 # chrome doesn't handle stress well apparently
chromium --app="https://discordapp.com/channels/@me" &
