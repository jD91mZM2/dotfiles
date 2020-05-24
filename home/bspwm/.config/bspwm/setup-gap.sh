#!/bin/sh

window_gap="${1:?window gap must be set}"
preferred_gap="${2:-12}"
polybar_height="${3:-30}"
# default of 30 is obtained from `xprop | grep _NET_WM_STRUT` on polybar

border="$((preferred_gap - window_gap))"
top="$((polybar_height + border))"

bspc config  window_gap      "$window_gap"
bspc config  top_padding     "$top"
bspc config  bottom_padding  "$border"
bspc config  left_padding    "$border"
bspc config  right_padding   "$border"
