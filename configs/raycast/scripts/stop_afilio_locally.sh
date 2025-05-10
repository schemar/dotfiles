#!/usr/bin/env bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Stop Emulators
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🤖
# @raycast.packageName Afilio

# Layout:
# 1 2 4
# 1 3 5
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.1 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.2 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.4 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.3 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.5 C-c
