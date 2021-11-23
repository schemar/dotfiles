#!/bin/bash

# This file starts kmonad for all keyboards that it can find.

default="/dev/input/by-id/usb-Microsoft_Microsoft®_Nano_Transceiver_v2.1-event-kbd"
sculpt_home="/dev/input/by-id/usb-Microsoft_Microsoft®_Nano_Transceiver_v2.1-event-kbd"
sculpt_office="/dev/input/by-id/usb-Microsoft_Microsoft®_2.4GHz_Transceiver_v9.0-event-kbd"
lenovo="/dev/input/by-path/platform-i8042-serio-0-event-kbd"

start_kmonad() {
    device=$1

    tmp_file="$(mktemp).kbd"
    cp "$HOME/.config/kmonad/config.kbd" "${tmp_file}"
    sed -i "s:${default}:${device}:" $tmp_file

    kmonad $tmp_file &
}

pkill kmonad
rm /tmp/tmp.*.kbd

if [[ -e "${sculpt_home}" ]]; then
    start_kmonad "${sculpt_home}"
fi

if [[ -e "${sculpt_office}" ]]; then
    start_kmonad "${sculpt_office}"
fi

if [[ -e "${lenovo}" ]]; then
    start_kmonad "${lenovo}"
fi
