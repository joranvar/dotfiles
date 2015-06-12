#! /usr/bin/env zsh

mypath=${0:a:h}

for i in antigen xmobarrc xmonad zshrc; do
    [[ -a ~/.$i ]] || ln -s $mypath/$i ~/.$i
done
