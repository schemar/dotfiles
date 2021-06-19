#!/bin/bash

# Kill
killall -q picom

## Wait until the processes have been shut down
while pgrep -u $UID -x picom >/dev/null; do sleep 1; done

# Launch
DISPLAY=":0" picom -b --experimental-backends --backend glx
