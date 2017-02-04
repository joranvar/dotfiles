#! /usr/bin/env zsh

mypath=${0:a:h}

for i in antigen config/git config/mpd conkerorrc emacs.d/init.el emacs.d/lisp gnupg/scdaemon.conf stack/config.yaml xmobarrc xmonad/xmonad.hs xmonad/.conky_dzen xprofile zshrc; do
    echo $mypath/$i "->" ~/.$i
    mkdir -p ~/.${i:h}
    [[ -a ~/.$i ]] || ln -s $mypath/$i ~/.$i
done
