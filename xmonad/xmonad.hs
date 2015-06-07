import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import XMonad.Util.SpawnOnce(spawnOnce)

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
	, startupHook = spawnOnce "/usr/bin/env xscreensaver -no-splash"
        } `additionalKeys`
        ( [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
          ] ++
          [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                                             , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ] )
