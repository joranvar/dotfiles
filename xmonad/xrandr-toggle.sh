#! /usr/bin/env zsh

typeset -A notifications

pkill taffybar

export TAFFY_SCREEN=0
taffybar&
screens=("eDP1" "LVDS1")
for s in $screens ; do
    mode=$(xrandr -q | sed '1,/'$s' connected/d;/.* connected/,$d' | head -n 1 | cut -d' ' -f4)
    if [[ -n $mode ]]; then
        echo "Setting $mode for $s."
        notifications+="$s $mode\n"
        xrandr --output $s --mode $mode
        prev=$s
    fi
done

if [[ -z $prev ]]; then
    echo "No laptop screen found, not doing anything"
    exit
fi

screens=$(xrandr -q | grep ".* disconnected" | cut -f1 -d' ' | grep -v "eDP1\\|LVDS1")
for s in $(echo $screens) ; do
    echo $s off
    notifications+="$s off\n"
    xrandr --output $s --off
done

screens=$(xrandr -q | grep ".* connected" | cut -f1 -d' ' | grep -v "eDP1\\|LVDS1")
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
