#! /usr/bin/env zsh

mode="2560x1440"

count=$(xrandr -q | grep -c "HDMI1 connected")
if [[ $count -gt 0 ]]; then
    echo Two screens
    xrandr --output HDMI1 --auto --left-of eDP1 --mode $mode
else
    echo One screen
    xrandr --output HDMI1 --off
fi

pkill -USR1 xmobar
pkill trayer
(nohup trayer --SetPartialStrut true --edge top --align right --width 10 --height 14 --transparent true --alpha 0 --tint black > /dev/null 2> /dev/null)&
