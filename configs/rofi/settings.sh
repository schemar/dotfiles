#!/bin/bash

# Options
display=" Display"
scaling=" Scaling"
audio="蓼 Audio"
audio_rescan="遼 Rescan audio devices"
network=" Network"

options="${network}\n${display}\n${scaling}\n${audio}\n${audio_rescan}"

selected="$(echo -e ${options} | rofi -dmenu -i -p ' ' -theme slate_nord)"

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
    $network)
        kitty --title "settings-nmtui-connect" nmtui-connect
        ;;
    $scaling)
        ~/.config/rofi/scaling.sh
        ;;
esac
