#!/bin/sh

dir="$(dirname "$0")"

xautolock \
  -time 1 \
  -locker "$dir/lock.sh" \
  -secure \
  -notify 10 \
  -notifier "$dir/dim.sh"
