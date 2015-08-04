#! /usr/bin/env zsh

mypath=${0:a:h}

for i in antigen config/git emacs.d/init.el emacs.d/lisp xmobarrc xmonad/xmonad.hs zshrc; do
    mkdir -p ~/.${i:h}
    [[ -a ~/.$i ]] || ln -s $mypath/$i ~/.$i
done
