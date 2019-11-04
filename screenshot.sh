#!/usr/bin/env bash

set -e

case "$1" in
    screen)
        # -----------------------------------------------------------------------------------
        # COPY PASTED FROM
        # SOURCE: https://gist.github.com/naelstrof/f9b74b5221cdc324c0911c89a47b8d97
        # FORK:   https://gist.github.com/jD91mZM2/99cfe5e74e34a08f4b0854c82f9bc0b3
        # ALL CREDITS GO TO THE CREATOR OF THE SOURCE SCRIPT
        # -----------------------------------------------------------------------------------
        monitors="$(xrandr | grep -o '[0-9]*x[0-9]*[+-][0-9]*[+-][0-9]*')"
        # Get the location of the mouse
        eval "$(xdotool getmouselocation --shell --prefix mouse_)"
        eval "$(xdotool getmouselocation --shell --prefix mouse_)"

        while read -r line; do
            if [[ "$line" =~ ([0-9]+)x([0-9]+)([+-][0-9]+)([+-][0-9]+) ]]; then
                specs="${BASH_REMATCH[0]}"
                width="${BASH_REMATCH[1]}"
                height="${BASH_REMATCH[2]}"
                x="${BASH_REMATCH[3]}"
                y="${BASH_REMATCH[4]}"
                echo "$width * $height + $offset_x + $offset_y"

                if (( $mouse_X >= $x )) && (( $mouse_X <= $x+$width )) \
                       && (( $mouse_Y >= $y )) && (( $mouse_Y <= $y+$height )); then
                    import -window root -crop "$specs" /tmp/screenshot.png
                    break
                fi
            fi
        done < <(xrandr)
        # -----------------------------------------------------------------------------------
        ;;
    window)
        import -window "$(xdotool getwindowfocus -f)" /tmp/screenshot.png
        ;;
    region)
        import /tmp/screenshot.png
        ;;
    *)
        echo "Invalid argument. Must be one of screen, window or region." >&2
        exit
        ;;
esac

xclip -sel clip -t image/png /tmp/screenshot.png
mpv /tmp/screenshot.png
