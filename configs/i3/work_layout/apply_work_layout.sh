#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" || exit ; pwd -P )"

for i in $(seq 1 6)
do
    i3-msg "workspace ${i}; append_layout $SCRIPTPATH/work_layout_${i}.json"
done

i3-msg "workspace 10; append_layout $SCRIPTPATH/work_layout_10.json"

sleep 1

nohup emacs 1>/dev/null 2>&1 &
nohup google-chrome 1>/dev/null 2>&1 &
nohup firefox 1>/dev/null 2>&1 &
nohup emacs 1>/dev/null 2>&1 &
nohup slack 1>/dev/null 2>&1 &
nohup flatpak run com.spotify.Client 2>&1 &
nohup flatpak run com.discordapp.Discord 2>&1 &
