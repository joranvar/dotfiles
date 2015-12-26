import System.IO
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Volume
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
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
    xmonad $ ewmh $ defaultConfig
        { modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmobar
            , ppTitle = xmobarColor "green" "" . shorten 100
            }
        , startupHook = do
            spawnOnce "/usr/bin/env xscreensaver -no-splash"
            spawnOnce "trayer --SetPartialStrut true --edge top --align right --width 10 --height 14 --transparent true --alpha 0 --tint black"
            spawnOnce "nm-applet"
        , terminal = "/usr/bin/env emacsclient -c -n -e \"(eshell \\\"@login\\\")\""
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        } `additionalKeys`
        ( [ ((mod4Mask .|. shiftMask, xK_l                    ), spawn "xscreensaver-command -lock")
          , ((mod4Mask,               xK_b                    ), sendMessage ToggleStruts)
          , ((mod4Mask,               xK_p                    ), spawn "dmenu_run -b")
          , ((0,                      xF86XK_AudioLowerVolume ), lowerVolume 6 >> return ())
          , ((0,                      xF86XK_AudioRaiseVolume ), raiseVolume 3 >> return ())
          , ((0,                      xF86XK_AudioMute        ), toggleMute >> return ())
          , ((mod4Mask,               xK_r                    ), spawn "pkill redshift || redshift -l 50.9342277:-5.7725223")
          ] ++
          [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e] [0..]
                                             , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ] )
