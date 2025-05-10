#!/usr/bin/env bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Start Emulators
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🤖
# @raycast.packageName Afilio

./stop_afilio_locally.sh
sleep 10

# Layout:
# 1 2 4
# 1 3 5
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.1 'npm run start' C-m
# Sleeping and waiting for npm run start before starting emulators ...
sleep 30

/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.2 'npm run emulator:firebase' C-m
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.4 'npm run emulator:functions' C-m
# Sleeping and waiting for firebase and functions before starting temporal server ...
sleep 20

/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.3 'npm run emulator:temporal-server | grep -v Gecko' C-m
# Sleeping and waiting for temporal server before starting worker ...
sleep 5

/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.5 'npm run emulator:temporal-worker' C-m
sleep 10

open http://localhost:3005/namespaces/default/workflows
open http://localhost:3001/
open http://admin.emulator.localhost:3000/users/
open http://emulator.localhost:3000/app/home/

osascript -e 'display notification "Success" with title "Emulators started"'
echo "Done. Emulators are running."
