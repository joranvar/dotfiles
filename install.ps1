Function New-Symlink ($link, $target)
{
    if (Test-Path -Pathtype container $target)
    {
        $command = "/c mklink /d"
    }
    else
    {
        $command = "/c mklink"
    }

    Start-Process cmd -Verb RunAs -ArgumentList "$command $link $target"
}

$myPath = Split-Path -Path $MyInvocation.MyCommand.Definition -Parent

Foreach ($i in @("antigen","config\git","emacs.d\init.el","emacs.d\lisp","xmobarrc","xmonad\xmonad.hs","zshrc")) {
    $target = $env:UserProfile + "\." + $i
    if (Test-Path -Pathtype container $i)
    {
        $targetFolder = $i
    }
    else
    {
        $targetFolder = Split-Path -Path $target -Parent
    }

    New-Item -Path $targetFolder -Type directory -Force

    echo $target
    if (! (Test-Path $target))
    {
        New-Symlink $target ($myPath + "\" + $i)
    }
}
