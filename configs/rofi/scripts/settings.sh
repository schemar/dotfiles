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
printers="Printers (opens in browser; username/password like linux)"
printers_icon="\0icon\x1fgnome-dev-printer"
power="Power"
power_icon="\0icon\x1fbattery"

options="${network}${network_icon}\n${audio}${audio_icon}\n${power}${power_icon}\n${display}${display_icon}\n${scaling}${scaling_icon}\n${audio_rescan}${audio_rescan_icon}\n${printers}${printers_icon}"

selected="$(echo -en ${options} | rofi -dmenu -i -p ' ' -theme icons)"

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
    $power)
        xfce4-power-manager-settings
        ;;
    $scaling)
        ~/.config/rofi/scripts/scaling.sh
        ;;
    $printers)
        qutebrowser http://localhost:631/admin
        ;;
esac
