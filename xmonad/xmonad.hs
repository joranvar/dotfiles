import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

main = do
    xmobar <- spawnPipe "/usr/bin/env xmobar -x 1"
    xmonad $ defaultConfig
        { modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmobar
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
        ]
