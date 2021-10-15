#!/bin/bash

tmp_image() {
    image="$(mktemp)"
    mv "${image}"{,.png}
    image="${image}.png"
    echo $image
}

screenshot="$(tmp_image)"
blurred="$(tmp_image)"

maim "$screenshot"
# Speedup over simple `-blur` as described in
# https://legacy.imagemagick.org/Usage/blur/#blur_args
convert "$screenshot" \
    -filter Gaussian \
    -resize 25% \
    -define filter:sigma=5 \
    -resize 400% \
    "$blurred"

i3lock -i "$blurred"

rm -f "$screenshot" "$blurred"
