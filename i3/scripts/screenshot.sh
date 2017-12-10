#!/bin/sh

case "$1" in
    screen)
        # -----------------------------------------------------------------------------------
        # COPY PASTED FROM https://gist.github.com/naelstrof/f9b74b5221cdc324c0911c89a47b8d97
        # ALL CREDIT TO HIM
        # -----------------------------------------------------------------------------------
        monitors=$(xrandr | grep -o '[0-9]*x[0-9]*[+-][0-9]*[+-][0-9]*')
        # Get the location of the mouse
        xmouse=$(xdotool getmouselocation | awk -F "[: ]" '{print $2}')
        ymouse=$(xdotool getmouselocation | awk -F "[: ]" '{print $4}')

        for mon in ${monitors}; do
            # Parse the geometry of the monitor
            monw=$(echo ${mon} | awk -F "[x+]" '{print $1}')
            monh=$(echo ${mon} | awk -F "[x+]" '{print $2}')
            monx=$(echo ${mon} | awk -F "[x+]" '{print $3}')
            mony=$(echo ${mon} | awk -F "[x+]" '{print $4}')
            # Use a simple collision check
            if (( ${xmouse} >= ${monx} )); then
                if (( ${xmouse} <= ${monx}+${monw} )); then
                    if (( ${ymouse} >= ${mony} )); then
                        if (( ${ymouse} <= ${mony}+${monh} )); then
                            # We have found our monitor!
                            maim -g "${monw}x${monh}+${monx}+${mony}" /tmp/screenshot
                        fi
                    fi
                fi
            fi
        done
        # -----------------------------------------------------------------------------------
        ;;
    window)
        maim -i "$(xdotool getactivewindow)" /tmp/screenshot
        ;;
    region)
        maim -s /tmp/screenshot
        ;;
    *)
        echo "Invalid argument. Must be one of screen, window or region." >&2
        exit
        ;;
esac

xclip -sel clip -t image/png /tmp/screenshot
mpv /tmp/screenshot
rm /tmp/screenshot
