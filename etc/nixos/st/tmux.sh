#!/bin/sh

tmux attach -t main \
    || tmux new -s main
