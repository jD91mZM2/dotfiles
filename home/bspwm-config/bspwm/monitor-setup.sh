#!/usr/bin/env bash

readarray monitors    < <(xrandr | awk '/\<connected\>/ { print $1 }')
readarray geometries  < <(xrandr | awk '/\<connected primary\>/ { print $4; next } /\<connected\>/ { print $3 }')

# Update monitors in BSPWM (it doesn't detect mine correctly automatically)
while [ $(bspc query -M | wc -l) -gt "${#monitors[@]}" ]; do
    bspc monitor primary#next -r
done
i=$(bspc query -M | wc -l)
while [ $i -lt "${#monitors[@]}" ]; do
    bspc wm --add-monitor ${monitors[$i]} ${geometries[$i]}
    i=$((i + 1))
done

# Update desktops
m1=("Terminal" "Editor" "Primary" "Secondary" "5" "6" "7")
m2=("Misc" "Message")
if [ -n "$(bspc query -M primary#next)" ]; then
    bspc monitor primary#next  -d ${m2[@]}
    bspc monitor primary       -d ${m1[@]}
else
    bspc monitor primary -d ${m1[@]} ${m2[@]}
fi

# Update background
feh --bg-fill ~/Pictures/Backgrounds/background.jpg --no-fehbg

# Adopt nodes that aren't on any desktops now
bspc desktop -f "^9"
bspc wm -o

# Restart polybars
systemctl restart --user polybar
