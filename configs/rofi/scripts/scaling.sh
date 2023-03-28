#!/bin/bash

# Options
one="1.0"
onefive="1.5"
two="2.0"

options="${one}\n${onefive}\n${two}"

selected="$(echo -e ${options} | rofi -dmenu -i -p '󰍹 ' -theme no_icons)"

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

# Create Xresources if it doesn't exist
[[ -f ~/.Xresources ]] || printf "Xft.dpi: ${dpi}\nrofi.dpi: ${dpi}" > ~/.Xresources

# Add line if file exists but line is missing
if ! grep -Fq "Xft.dpi: " ~/.Xresources; then echo "Xft.dpi: ${dpi}" >> ~/.Xresources; fi
if ! grep -Fq "rofi.dpi: " ~/.Xresources; then echo "rofi.dpi: ${dpi}" >> ~/.Xresources; fi

# Update dpi in Xresources
sed -i "s/Xft.dpi: .*/Xft.dpi: ${dpi}/" ~/.Xresources
sed -i "s/rofi.dpi: .*/rofi.dpi: ${dpi}/" ~/.Xresources

yes="Yes"
no="No"
options="${yes}\n${no}"
selected="$(echo -e ${options} | rofi -dmenu -i -p 'Exit? ' -theme no_icons_prompt)"
case $selected in
    $yes)
        i3-msg exit
        ;;
esac
