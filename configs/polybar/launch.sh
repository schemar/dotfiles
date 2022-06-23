#!/usr/bin/env bash

# Terminate already running bar instances
polybar-msg cmd quit
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bar(s)
echo "---" | tee -a /tmp/polybar1.log
polybar main -c $HOME/.config/polybar/config.ini 2>&1 | tee -a /tmp/polybar_main.log & disown

echo "Bars launched..."
