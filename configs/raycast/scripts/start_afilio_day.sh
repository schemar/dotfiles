#!/bin/bash

# Raycast Script Command Template
#
# Duplicate this file and remove ".template." from the filename to get started.
# See full documentation here: https://github.com/raycast/script-commands
#
# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Start Afilio Day
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon ☀️
# @raycast.packageName Afilio

# Use the setup layout which starts all apps.
open -g "raycast://customWindowManagementCommand?&name=Start Afilio Day"

# Log into all the things "security key":
open https://console.cloud.google.com/
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.2 'gcloud auth login --enable-gdrive-access --update-adc' C-m
/etc/profiles/per-user/$USER/bin/tmux send-keys -t afilio:2.4 'firebase login --reauth' C-m

# Upgrade all the things:
/etc/profiles/per-user/$USER/bin/tmux send-keys -t dots:1.1 'just brew-upgrade' C-m

echo "All started. Logging into gcloud."
