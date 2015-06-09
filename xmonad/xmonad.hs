import System.IO
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.NoBorders ( noBorders, smartBorders )
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce(spawnOnce)

myLayout = avoidStruts  $  smartBorders tiled ||| smartBorders (Mirror tiled) ||| noBorders Full
  where
    tiled = Tall nmaster delta tiled_ratio
    nmaster = 1
    delta = 3/100
    tiled_ratio = 1/2

main = do
    xmobar <- spawnPipe "/usr/bin/env xmobar -x 1"
    xmonad $ defaultConfig
        { modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmobar
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
	, startupHook = do
	    spawnOnce "/usr/bin/env xscreensaver -no-splash"
	    spawnOnce "trayer --SetPartialStrut true --edge top --align right --width 10 --height 14"
	    spawnOnce "nm-applet"
	, terminal = "/usr/bin/env urxvt"
        } `additionalKeys`
        ( [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
          , ((mod4Mask,               xK_b), sendMessage ToggleStruts)
          ] ++
          [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                                             , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ] )
