#!/bin/bash

# Scale interface up or down based on current resolution.
# When scaled, the reported resolution scales with it.
# This means that we can always know from the current resolution if we need to
# scale up or down.

UPPER_BREAK="2000"
LOWER_BREAK="1000"
WIDTH=$(xrandr | grep -E -o 'current [0-9]+ x [0-9]+' | awk '{print$2}')
CURRENT_SCALE=$(swaymsg -t get_outputs | jq -r '.[] | select(.focused)' | jq -r '.scale')
NEW_SCALE=$CURRENT_SCALE

if [[ $WIDTH -gt $UPPER_BREAK ]]; then
    NEW_SCALE=$(echo "$CURRENT_SCALE * 2" | bc)
fi

if [[ $WIDTH -lt $LOWER_BREAK ]]; then
    NEW_SCALE=$(echo "$CURRENT_SCALE / 2" | bc)
fi


swaymsg -- output \* scale "$NEW_SCALE"
