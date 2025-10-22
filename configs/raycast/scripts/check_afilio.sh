#!/usr/bin/env bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Check Project
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🧑🏻‍🔬
# @raycast.packageName Afilio

# Layout:
# 1 2
# 1 3
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:3.1 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:3.1 'npx lefthook run lint; osascript -e "display notification \"Checking of Afilio project done in tmux.\" with title \"Check Done\""' C-m
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:3.2 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:3.2 'npx lefthook run tsc && npm run test' C-m
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:3.3 C-c
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:3.3 './misc/analyze-dependencies.sh' C-m
