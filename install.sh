#! /usr/bin/env zsh

mypath=${0:a:h}

for i in antigen config/git config/mpd config/taffybar conkerorrc emacs.d/init.el emacs.d/init.org.el emacs.d/init.org emacs.d/lisp gnupg/scdaemon.conf gnupg/gpg-agent.conf nixpkgs mozrepl-conkeror.js stack/config.yaml xmobarrc xmonad/xmonad.hs xmonad/default.nix xmonad/shell.nix xprofile zshrc; do
    echo $mypath/$i "->" ~/.$i
    mkdir -p ~/.${i:h}
    [[ -a ~/.$i ]] || ln -s $mypath/$i ~/.$i
done

# Install bin files
for i in bin/*; do
    echo $mypath/$i "->" ~/$i
    mkdir -p ~/${i:h}
    [[ -a ~/$i ]] || ln -s $mypath/$i ~/$i
done

# Compile xmonad for first use
cd ~/.xmonad && nix-shell --pure --command 'ghc --make xmonad.hs -i -ilib -fforce-recomp -v0 -o xmonad-x86_64-linux'

for i in nixos/configuration.nix nixos/yubikey-gpg.nix; do
    echo $mypath/$i "->" /etc/$i
    sudo mkdir -p /etc/${i:h}
    [[ -a /etc/$i ]] || sudo ln -s $mypath/$i /etc/$i
done

[[ -a /etc/nixos/local-machine.nix ]] || sudo ln -s $mypath/nixos/$(hostname).nix /etc/nixos/local-machine.nix
