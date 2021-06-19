#!/bin/bash

# Options
display="Display"
display_icon="\0icon\x1fcomputer"
scaling="Scaling"
scaling_icon="\0icon\x1fcom.github.cassidyjames.dippi"
audio="Audio"
audio_icon="\0icon\x1faudio-speakers"
audio_rescan="Rescan audio devices"
audio_rescan_icon="\0icon\x1faudio-speaker-right-testing"
network="Network"
network_icon="\0icon\x1fnetwork-wireless"

options="${network}${network_icon}\n${display}${display_icon}\n${scaling}${scaling_icon}\n${audio}${audio_icon}\n${audio_rescan}${audio_rescan_icon}"

selected="$(echo -en ${options} | rofi -dmenu -i -p ' ' -theme slate_nord)"

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
