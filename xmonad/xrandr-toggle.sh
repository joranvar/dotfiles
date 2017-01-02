#! /usr/bin/env zsh

typeset -A notifications

screens=$(xrandr -q | grep ".* disconnected" | cut -f1 -d' ' | grep -v "eDP1")
for s in $(echo $screens) ; do
    echo $s off
    notifications+="$s off\n"
    xrandr --output $s --off
done

screens=$(xrandr -q | grep ".* connected" | cut -f1 -d' ' | grep -v "eDP1")
for s in $(echo $screens) ; do
    mode=$(xrandr -q | sed '1,/'$s' connected/d;/.* connected/,$d' | head -n 1 | cut -d' ' -f4)
    echo $s $mode
    notifications+="$s $mode\n"
    xrandr --output $s --left-of eDP1 --mode $mode
done

notify-send -t 1000 "New Resolutions" ${notifications[*]}
pkill -USR1 xmobar
pkill trayer
(nohup trayer --SetPartialStrut true --edge top --align right --width 10 --height 14 --transparent true --alpha 0 --tint black > /dev/null 2> /dev/null)&
