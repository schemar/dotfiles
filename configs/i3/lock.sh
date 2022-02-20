#!/bin/bash

# Use feh background as lock background.
fehbg=$(grep -o "'.*'" ~/.fehbg | sed "s/'//g")

# Scale it to match the screen dimensions.
tmp_file=$(mktemp -t XXXXXXXXXX.png)
screen_width="$(xdpyinfo | grep dimensions | cut -d' ' -f7 | cut -dx -f1)"
convert "$fehbg" -scale "$screen_width" "$tmp_file"

i3lock -i "$tmp_file"

rm "$tmp_file"
