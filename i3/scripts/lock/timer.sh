#!/bin/sh

dir="$(dirname "$0")"

xidlehook \
  --time 5 \
  --timer "$dir/lock.sh" \
  --notify 10 \
  --notifier  'xrandr --output "$(xrandr | grep primary | cut -d " " -f 1)" --brightness .1' \
  --canceller 'xrandr --output "$(xrandr | grep primary | cut -d " " -f 1)" --brightness 1' \
  --not-when-fullscreen
