#!/bin/bash

# Kill
killall -q kmonad

## Wait until the processes have been shut down
while pgrep -u $UID -x kmonad >/dev/null; do sleep 1; done

# Launch
SCRIPTPATH="$( cd "$(dirname "$0")" || exit ; pwd -P )"
kmonad "$SCRIPTPATH"/sculpt.cfg &
kmonad "$SCRIPTPATH"/thinkpad.cfg &
