Function new-symlink ($link, $target)
{
    if (test-path -pathtype container $target)
    {
        $command = "cmd /c mklink /d"
    }
    else
    {
        $command = "cmd /c mklink"
    }

   invoke-expression "$command $link $target"
}

# mypath=${0:a:h} # TODO: current script path

foreach ($i in antigen,emacs.d/init.el,emacs.d/lisp,xmobarrc,xmonad/xmonad.hs,zshrc) {
    # TODO: get folder name
    # TODO: real home directory (e.g. C:\Users\${me}\)
    new-item -path ~/.${i:h} -type directory -force
    # TODO: check if exists
    [[ -a ~/.$i ]] || new-symlink $mypath/$i ~/.$i
}
