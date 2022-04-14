#!/bin/bash

# Use feh background as lock background.
fehbg=$(grep -o "'.*'" ~/.fehbg | sed "s/'//g")

# Scale it to match the screen dimensions.
tmp_file=$(mktemp -t XXXXXXXXXX.png)
screen_dimensions="$(xdpyinfo | grep dimensions | cut -d' ' -f7 )"
convert "$fehbg" -scale "$screen_dimensions" -gravity center -background black -extent "$screen_dimensions" "$tmp_file"

i3lock -i "$tmp_file"

rm "$tmp_file"
