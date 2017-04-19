{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad (void)
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)

-- Docks
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docksStartupHook, manageDocks, ToggleStruts(..))

-- Full Screen Events
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

-- Window layout
import XMonad (Tall(..), Mirror(..), Full(..), (|||), doShift, className, (=?), (-->))
import XMonad.Layout.NoBorders (noBorders, smartBorders)

-- Taffybar
import System.Taffybar.Hooks.PagerHints (pagerHints)

-- Volume control
import XMonad.Actions.Volume (lowerVolume, raiseVolume, toggleMute)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute)

-- Events
import XMonad (spawn, sendMessage)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Keys
import XMonad (mod4Mask, (.|.), shiftMask, xK_l, xK_b, xK_p, xK_q, xK_x, xK_w, xK_r, xK_e, xK_s, KeySym, ButtonMask)
import XMonad.Util.EZConfig (additionalKeys)

-- Main
import XMonad (xmonad, XConfig(..), (<+>))
import Data.Default (def)

main :: IO ()
main = do
  xmonad $ myConfig def

myConfig :: XConfig a -> XConfig _
myConfig =
  ewmh
  . pagerHints
  . myModMask
  . myManageHook
  . applySteamSettings
  . myEventHook
  . myLayoutHook
  . myWorkSpaces
  . applyScreensaver (mod4Mask .|. shiftMask, xK_l) . myStartupHook
  . applyScreenshot (mod4Mask, xK_s)
  . myTerminal
  . myKeys

myModMask :: XConfig a -> XConfig a
myModMask x = x { modMask = mod4Mask }

myManageHook :: XConfig a -> XConfig a
myManageHook x = x { manageHook = manageDocks <+> manageHook x }

applySteamSettings :: XConfig a -> XConfig a
applySteamSettings x = x { manageHook = className =? "hl2_linux" --> doShift "two" <+> manageHook x }

myEventHook :: XConfig a -> XConfig a
myEventHook x = x { handleEventHook = docksEventHook <+> fullscreenEventHook <+> handleEventHook x }

myLayoutHook :: XConfig a -> XConfig _
myLayoutHook x = x { layoutHook = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled) ||| noBorders Full }
  where
    tiled = Tall nmaster delta tiled_ratio
    nmaster = 1
    delta = 3/100
    tiled_ratio = 1/2

myWorkSpaces :: XConfig a -> XConfig a
myWorkSpaces x = x { workspaces = ["i", "ii", "iii", "iv", "v", "vi"] }

myStartupHook :: XConfig a -> XConfig a
myStartupHook x = x { startupHook = docksStartupHook <+> mapM_ spawnOnce startupCommands }
  where
    startupCommands =
      [ "nm-applet"
      ]

myTerminal :: XConfig a -> XConfig a
myTerminal x = x { terminal = "/usr/bin/env termite" }

myKeys :: XConfig a -> XConfig a
myKeys = flip additionalKeys $
   [ ((mod4Mask,               xK_b                    ), sendMessage ToggleStruts)
   , ((mod4Mask,               xK_p                    ), spawn "exec $(yeganesh -x -- -nb orange -nf '#444' -sb yellow -sf black -fn Monospace-9:normal)")                     -- Run command
   , ((mod4Mask .|. shiftMask, xK_p                    ), spawn "termite -e $(yeganesh -x -- -nb orange -nf '#444' -sb yellow -sf black -fn Monospace-9:normal)") -- Run command in terminal
   , ((mod4Mask,               xK_x                    ), spawn "~/dotfiles/xmonad/xrandr-toggle.sh")
   , ((0,                      xF86XK_AudioLowerVolume ), void $ lowerVolume 6)
   , ((0,                      xF86XK_AudioRaiseVolume ), void $ raiseVolume 3)
   , ((0,                      xF86XK_AudioMute        ), void toggleMute)
   , ((mod4Mask,               xK_r                    ), spawn "pkill redshift || redshift -l 50.9342277:-5.7725223")
   , ((mod4Mask,               xK_q                    ), spawn "cd ~/.xmonad ; nix-shell --pure --command 'ghc --make xmonad.hs -i -ilib -fforce-recomp -v0 -o xmonad-x86_64-linux' && xmonad --restart")
   ] ++
   [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                                      , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ]

applyScreensaver :: (ButtonMask, KeySym) -> XConfig a -> XConfig a
applyScreensaver lockKey = addLockKey lockKey . addStartup
  where
    addLockKey key x = x `additionalKeys` [ (key, spawn "xscreensaver-command -lock") ]
    addStartup x = x { startupHook = spawnOnce "/usr/bin/env xscreensaver -no-splash" >> startupHook x }

applyScreenshot :: (ButtonMask, KeySym) -> XConfig a -> XConfig a
applyScreenshot scrotKey = addScrotKey scrotKey
  where
    addScrotKey key x = x `additionalKeys` [ (key, spawn "scrot -u -e 'gimp $f'") ]
