#!/bin/bash

# Options
lock="Lock"
lock_icon="\0icon\x1fsystem-lock-screen"
suspend="Suspend"
suspend_icon="\0icon\x1fsystem-suspend"
power_off="Shutdown"
power_off_icon="\0icon\x1fsystem-shutdown"
reboot="Reboot"
reboot_icon="\0icon\x1fsystem-reboot"
exit="Exit"
exit_icon="\0icon\x1fsystem-log-out"

options="${lock}${lock_icon}\n${suspend}${suspend_icon}\n${power_off}${power_off_icon}\n${reboot}${reboot_icon}\n${exit}${exit_icon}"

selected="$(echo -en ${options} | rofi -dmenu -p ' ' -i -theme slate_nord)"

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
