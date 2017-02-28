#! /usr/bin/env zsh

typeset -A notifications

pkill taffybar

export TAFFY_SCREEN=0
taffybar&
screens=$(xrandr -q | grep ".* disconnected" | cut -f1 -d' ' | grep -v "eDP1")
for s in $(echo $screens) ; do
    echo $s off
    notifications+="$s off\n"
    xrandr --output $s --off
done

prev=eDP1
screens=$(xrandr -q | grep ".* connected" | cut -f1 -d' ' | grep -v "eDP1")
for s in $(echo $screens) ; do
    TAFFY_SCREEN=$(($TAFFY_SCREEN + 1))
    mode=$(xrandr -q | sed '1,/'$s' connected/d;/.* connected/,$d' | head -n 1 | cut -d' ' -f4)
    echo $s $mode
    notifications+="$s $mode\n"
    xrandr --output $s --left-of $prev --mode $mode
    taffybar&
    prev=$s
done

notify-send -t 1000 "New Resolutions" ${notifications[*]}
