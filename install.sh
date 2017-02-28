#! /usr/bin/env zsh

mypath=${0:a:h}

for i in antigen config/git config/mpd config/taffybar conkerorrc emacs.d/init.el emacs.d/lisp gnupg/scdaemon.conf mozrepl-conkeror.js stack/config.yaml xmobarrc xmonad/xmonad.hs xmonad/default.nix xmonad/shell.nix xprofile zshrc; do
    echo $mypath/$i "->" ~/.$i
    mkdir -p ~/.${i:h}
    [[ -a ~/.$i ]] || ln -s $mypath/$i ~/.$i
done
