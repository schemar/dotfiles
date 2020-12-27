#!/bin/bash

# Options
lock=" Lock"
suspend="鈴 Suspend"
power_off=" Power off"
reboot=" Reboot"
exit=" Exit"

options="${lock}\n${suspend}\n${power_off}\n${reboot}\n${exit}"

selected="$(echo -e ${options} | rofi -dmenu -i -p ' ' -theme slate_nord)"

case $selected in
    $lock)
        $HOME/.config/i3/lock.sh
        ;;
    $suspend)
        $HOME/.config/i3/lock.sh
        systemctl suspend
        ;;
    $power_off)
        systemctl poweroff -i
        ;;
    $reboot)
        systemctl reboot
        ;;
    $exit)
        i3-msg exit
        ;;
esac
