#!/bin/bash

screenshotname="$HOME/Pictures/$(date +'%s_screenshot.png')"

# Options
screen="Entire screen"
area="Area or window"
area_clip="Area or window to clipboard"

options="${screen}\n${area}\n${area_clip}"

selected="$(echo -e ${options} | rofi -dmenu -i -p ' ' -theme slate_nord)"

case $selected in
    $screen)
        maim $screenshotname
        ;;
    $area)
        maim -s $screenshotname
        ;;
    $area_clip)
        maim -s | xclip -selection clipboard -t image/png
        ;;
esac
