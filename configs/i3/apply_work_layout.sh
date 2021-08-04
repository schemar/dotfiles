#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" || exit ; pwd -P )"

for i in $(seq 1 5)
do
    i3-msg "workspace ${i}; append_layout $SCRIPTPATH/work_layout_${i}.json"
done
sleep 1

nohup kitty 1>/dev/null 2>&1 &
nohup google-chrome 1>/dev/null 2>&1 &
nohup qutebrowser 1>/dev/null 2>&1 &
nohup slack 1>/dev/null 2>&1 &
nohup teams 1>/dev/null 2>&1 &
nohup flatpak run md.obsidian.Obsidian 1>/dev/null 2>&1 &
