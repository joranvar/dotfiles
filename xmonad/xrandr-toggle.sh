#! /usr/bin/env zsh

typeset -A notifications

pkill taffybar

work=$(xrandr --verbose | grep 00ffffffffffff000472dd035f627040)
if [[ $work ]]; then
    nvidia-settings -l --config=~/tmp/xconfig-work
    export TAFFY_MAIN_SCREEN=2
else
    nvidia-settings -l --config=~/tmp/xconfig-home
    export TAFFY_MAIN_SCREEN=1
fi

active=$(xrandr -q | grep ".* connected" | wc -l)
for s in $(seq 0 $(($active - 1))); do
    echo $s
    export TAFFY_SCREEN=$s
    taffybar&
done
