#!/bin/bash

command=$(printf "Display settings:\tarandr\nAudio settings:  \tpavucontrol\nRescan audio devices:\trm -rf ~/.config/pulse && pulseaudio -k\nReload keyboard config:\tsetxkbmap -layout us  -option 'compose:menu' -option 'ctrl:nocaps' -variant altgr-intl && xset r rate 300 40\nSelect wifi:        \tkitty -e nmtui-connect" | rofi -dmenu -font "DejaVuSansMono Nerd Font 10" -theme slate_nord | awk 'BEGIN { FS="\t" } ; { print $2 }')
eval $command
