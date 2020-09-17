#!/bin/bash

tmp_image() {
    image="$(mktemp)"
    mv "${image}"{,png}
    image="${image}.png"
    echo $image
}

screenshot="$(tmp_image)"
blurred="$(tmp_image)"

maim "$screenshot"
convert "$screenshot" -blur 0x10 "$blurred"

i3lock -i "$blurred"

rm -f "$screenshot" "$blurred"
