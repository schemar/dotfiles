#!/bin/bash

# Options
display=" Display"
audio="蓼 Audio"
audio_rescan="遼 Rescan audio devices"
keyboard_reload=" Reload keyboard config"
network=" Network"

options="${network}\n${display}\n${audio}\n${audio_rescan}\n${keyboard_reload}"

selected="$(echo -e ${options} | rofi -dmenu -i -p 'Settings' -font 'DejaVuSansMono Nerd Font 10' -theme slate_nord)"

case $selected in
    $display)
        arandr
        ;;
    $audio)
        pavucontrol
        ;;
    $audio_rescan)
        rm -rf ~/.config/pulse && pulseaudio -k
        ;;
    $keyboard_reload)
        setxkbmap -layout us  -option 'compose:menu' -option 'ctrl:nocaps' -variant altgr-intl && xset r rate 300 40
        ;;
    $network)
        kitty --title "settings-nmtui-connect" nmtui-connect
        ;;
esac
