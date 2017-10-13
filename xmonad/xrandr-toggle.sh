#! /usr/bin/env zsh

typeset -A notifications

pkill taffybar

export TAFFY_SCREEN=0
if [[ $(xrandr -q | grep ".* connected" | wc -l) -eq 1 ]]; then
    export TAFFY_MAIN_SCREEN=0
else
    export TAFFY_MAIN_SCREEN=1
fi

order=($*)
if [[ -z $order ]]; then
    order=("HDMI1" "eDP1" "VGA1")
fi

active=$(xrandr -q | grep ".* connected" | cut -f1 -d' ')
inactive=$(xrandr -q | grep ".* disconnected" | cut -f1 -d' ')

for s in $order ; do
    echo $s ${active[(I)$s]}
    if [[ ${active[(I)$s]} -gt 0 ]]; then
        mode=$(xrandr -q | sed '1,/'$s' connected/d;/.* connected/,$d' | head -n 1 | cut -d' ' -f4)
        echo $s $mode
        notifications+="$s $mode\n"
        echo $s ${order[(I)$s]}
        if [[ ${order[(I)$s]} -eq 1 ]]; then
            xrandr --output $s --mode $mode
        else
            xrandr --output $s --right-of $prev --mode $mode
        fi
        taffybar&
        prev=$s
        TAFFY_SCREEN=$(($TAFFY_SCREEN + 1))
    fi
done

for s in $(echo $inactive) ; do
    TAFFY_SCREEN=$(($TAFFY_SCREEN + 1))
    mode=$(xrandr -q | sed '1,/'$s' connected/d;/.* connected/,$d' | head -n 1 | cut -d' ' -f4)
    echo $s $mode
    notifications+="$s off\n"
    xrandr --output $s --off
done

notify-send -t 1000 "New Resolutions" ${notifications[*]}
