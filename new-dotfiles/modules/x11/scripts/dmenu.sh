#!/bin/sh

@dmenu@ -i -l 15 \
      -fn Hack-10 \
      -nb "#@base0@" -sb "#@base2@" \
      -nf "#@base5@" -sf "#@base7@" \
      "$@"
