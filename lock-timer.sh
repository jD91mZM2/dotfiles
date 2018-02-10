#!/bin/sh

if [ -z "$1" ]; then
    echo "No timer command supplied. Exiting..."
    exit
fi

dir="$(dirname "$0")"

rm /tmp/xidlehook.sock

read -r -d '' lock << EOF
    # Undim screen
    xrandr --output "\$(xrandr | grep primary | cut -d ' ' -f 1)" --brightness 1

    # Pause notifications
    pkill dunst -USR1

    $1

    # Start notifications
    pkill dunst -USR2
EOF

xidlehook \
  --time 1 \
  --timer "$lock" \
  --notify 10 \
  --notifier  'xrandr --output "$(xrandr | grep primary | cut -d " " -f 1)" --brightness .1' \
  --canceller 'xrandr --output "$(xrandr | grep primary | cut -d " " -f 1)" --brightness 1' \
  --not-when-fullscreen \
  --not-when-audio \
  --socket /tmp/xidlehook.sock
