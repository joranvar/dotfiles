{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad (void)
import System.IO (hPutStrLn, Handle)
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)

-- Docks
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))

-- Full Screen Events
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

-- Window layout
import XMonad (Tall(..), Mirror(..), Full(..), Window, (|||))
import XMonad.Layout.NoBorders (noBorders, smartBorders)

-- XMobar
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppTitle, xmobarColor, shorten)

-- Volume control
import XMonad.Actions.Volume (lowerVolume, raiseVolume, toggleMute)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute)

-- Events
import XMonad (spawn, sendMessage)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Keys
import XMonad (mod4Mask, (.|.), shiftMask, xK_l, xK_b, xK_p, xK_x, xK_w, xK_r, xK_e)
import XMonad.Util.EZConfig (additionalKeys)

-- Main
import XMonad (xmonad, XConfig(..), (<+>))
import Data.Default (def)

main :: IO ()
main = do
  logHandle <- spawnPipe "/usr/bin/env xmobar -x 1"
  xmonad $ myConfig logHandle def

myConfig :: Handle -> XConfig a -> XConfig _
myConfig logHandle =
  ewmh
  . myModMask
  . myManageHook
  . myEventHook
  . myLayoutHook
  . myLogHook logHandle
  . myStartupHook
  . myTerminal
  . myKeys

myModMask :: XConfig a -> XConfig a
myModMask x = x { modMask = mod4Mask }

myManageHook :: XConfig a -> XConfig a
myManageHook x = x { manageHook = manageDocks <+> manageHook x }

myEventHook :: XConfig a -> XConfig a
myEventHook x = x { handleEventHook = fullscreenEventHook <+> handleEventHook x }

myLayoutHook :: XConfig a -> XConfig _
myLayoutHook x = x { layoutHook = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled) ||| noBorders Full }
  where
    tiled = Tall nmaster delta tiled_ratio
    nmaster = 1
    delta = 3/100
    tiled_ratio = 1/2

myLogHook :: Handle -> XConfig a -> XConfig a
myLogHook logHandle x = x { logHook = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn logHandle
                            , ppTitle = xmobarColor "green" "" . shorten 100
                            } }

myStartupHook :: XConfig a -> XConfig a
myStartupHook x = x { startupHook = mapM_ spawnOnce
                      [ "/usr/bin/env xscreensaver -no-splash"
                      , "trayer SetPartialStrut true edge top align right width 10 height 14 transparent true alpha 0 tint black"
                      , "nm-applet" ] }

myTerminal :: XConfig a -> XConfig a
myTerminal x = x { terminal = "/usr/bin/env emacsclient -c -n -e \"(eshell \\\"@login\\\")\"" }

myKeys :: XConfig a -> XConfig a
myKeys = flip additionalKeys
  ( [ ((mod4Mask .|. shiftMask, xK_l                    ), spawn "xscreensaver-command -lock")
    , ((mod4Mask,               xK_b                    ), sendMessage ToggleStruts)
    , ((mod4Mask,               xK_p                    ), spawn "dmenu_run -b")
    , ((mod4Mask,               xK_x                    ), spawn "~/dotfiles/xmonad/xrandr-toggle.sh")
    , ((0,                      xF86XK_AudioLowerVolume ), void $ lowerVolume 6)
    , ((0,                      xF86XK_AudioRaiseVolume ), void $ raiseVolume 3)
    , ((0,                      xF86XK_AudioMute        ), void toggleMute)
    , ((mod4Mask,               xK_r                    ), spawn "pkill redshift || redshift -l 50.9342277:-5.7725223")
    ] ++
    [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e] [0..]
                                       , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ] )
