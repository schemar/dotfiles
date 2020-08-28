#!/bin/bash

# If $WIDTH is greater than $BREAK, set interface scaling to 2.

BREAK="2000"
WIDTH=$(xrandr | grep -E -o 'current [0-9]+ x [0-9]+' | awk '{print$2}')

if [[ $WIDTH -gt $BREAK ]]; then
    swaymsg -- output \* scale 2
else
    swaymsg -- output \* scale 1
fi
