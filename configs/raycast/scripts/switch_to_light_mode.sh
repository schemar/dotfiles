#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Switch to Light Mode
# @raycast.mode silent

# Optional parameters:
# @raycast.icon ☀️

# Documentation:
# @raycast.description Switches macOS and the terminal to light mode
# @raycast.author schemar
# @raycast.authorURL https://raycast.com/schemar

cd ~/Projects/dotfiles && /etc/profiles/per-user/$USER/bin/just theme-light
