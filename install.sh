#! /usr/bin/env zsh

mypath=${0:a:h}

for i in antigen emacs.d/init.el xmobarrc xmonad/xmonad.hs zshrc; do
    mkdir -p ~/.${i:h}
    [[ -a ~/.$i ]] || ln -s $mypath/$i ~/.$i
done
