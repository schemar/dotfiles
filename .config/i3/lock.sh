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
# Scaling improves speed while the blurred image looks ok.
# Without scaling, locking could take up to multiple secconds on HiDPI screens.
convert -scale 5% -blur 0x1 -resize 2000% "$screenshot" "$blurred"

i3lock -i "$blurred"

rm -f "$screenshot" "$blurred"
