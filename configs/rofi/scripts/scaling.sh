#!/bin/bash

# Options
one="1.0"
onefive="1.5"
two="2.0"

options="${one}\n${onefive}\n${two}"

selected="$(echo -e ${options} | rofi -dmenu -i -p ' ' -theme no_icons)"

case $selected in
    $one)
        dpi=96
        ;;
    $onefive)
        dpi=144
        ;;
    $two)
        dpi=192
        ;;
esac

# THIS OVERWRITES .Xresources !!!
echo "Xft.dpi: ${dpi}" > ~/.Xresources

yes="Yes"
no="No"
options="${yes}\n${no}"
selected="$(echo -e ${options} | rofi -dmenu -i -p 'Reboot? ' -theme no_icons_prompt)"
case $selected in
    $yes)
        systemctl reboot
        ;;
esac
