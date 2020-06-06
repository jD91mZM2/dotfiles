#!/bin/sh

dmenu -i -l 15 \
      -fn Hack-10 \
      -nb "#282c34" -sb "#3e4451" \
      -nf "#565c64" -sf "#abb2bf" \
      "$@"
