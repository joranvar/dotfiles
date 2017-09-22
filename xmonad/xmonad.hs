{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad (void)
import Control.Arrow (first)
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)

-- Docks
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docksStartupHook, manageDocks, docks, ToggleStruts(..))

-- Full Screen Events
import XMonad.Hooks.EwmhDesktops (ewmh)

-- Window layout
import XMonad (Tall(..), Mirror(..), Full(..), (|||), doShift, className, (=?), (-->))
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Grid (Grid(..))
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
import XMonad (mod4Mask, (.|.), shiftMask, xK_l, xK_b, xK_p, xK_q, xK_x, xK_w, xK_r, xK_e, xK_s, xK_f, KeySym, ButtonMask)
import XMonad.Util.EZConfig (additionalKeys)

-- WMName (for Java swing GUI)
import XMonad.Hooks.SetWMName (setWMName)

-- Main
import XMonad (xmonad, XConfig(..), (<+>))
import Data.Default (def)

main :: IO ()
main = do
  xmonad $ myConfig def

myConfig :: XConfig a -> XConfig _
myConfig =
  docks
  . pagerHints
  . fullscreenSupport
  . myModMask
  . myManageHook
  . applySteamSettings
  . myEventHook
  . myLayoutHook
  . myWorkSpaces
  . applyScreensaver (mod4Mask .|. shiftMask, xK_l)
  . myStartupHook
  . applyScreenshot (mod4Mask, xK_s)
  . myTerminal
  . myKeys
  . ewmh

myModMask :: XConfig a -> XConfig a
myModMask x = x { modMask = mod4Mask }

myManageHook :: XConfig a -> XConfig a
myManageHook x = x { manageHook = manageDocks <+> manageHook x }

applySteamSettings :: XConfig a -> XConfig a
applySteamSettings x = x { manageHook = className =? "hl2_linux" --> doShift "two" <+> manageHook x }

myEventHook :: XConfig a -> XConfig a
myEventHook x = x { handleEventHook = docksEventHook <+> handleEventHook x }

myLayoutHook :: XConfig a -> XConfig _
myLayoutHook x = x { layoutHook = avoidStruts $ (smartBorders tiled ||| smartBorders (Mirror tiled)) ||| noBorders Full ||| smartBorders (centerMaster Grid) }
  where
    tiled = Tall nmaster delta tiled_ratio
    nmaster = 1
    delta = 3/100
    tiled_ratio = 1/2

myWorkSpaces :: XConfig a -> XConfig a
myWorkSpaces x = x { workspaces = ["i", "ii", "iii", "iv", "v", "vi"] }

myStartupHook :: XConfig a -> XConfig a
myStartupHook x = x { startupHook = setWMName "LG3D" <+> mapM_ spawnOnce startupCommands <+> docksStartupHook }
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
   , ((mod4Mask .|. shiftMask, xK_b                    ), spawn "pkill redshift || redshift -l 50.9342277:-5.7725223")
   , ((mod4Mask,               xK_q                    ), spawn "cd ~/.xmonad ; nix-shell --pure --command 'ghc --make xmonad.hs -i -ilib -fforce-recomp -v0 -o xmonad-x86_64-linux' && xmonad --restart")
   , ((mod4Mask .|. shiftMask, xK_f                    ), spawn "~/bin/compton.set.focused_force $((($(~/bin/compton.get.focused_force | awk '{print $2}') + 1) % 3))")
   ] ++
   [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                                      , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ]

applyScreensaver :: (ButtonMask, KeySym) -> XConfig a -> XConfig a
applyScreensaver lockKey = addLockKey lockKey . addStartup
  where
    addLockKey key x = x `additionalKeys` [ (key, spawn "xscreensaver-command -lock") ]
    addStartup x = x { startupHook = spawnOnce "/usr/bin/env xscreensaver -no-splash" <+> startupHook x }

applyScreenshot :: (ButtonMask, KeySym) -> XConfig a -> XConfig a
applyScreenshot scrotKey = addScrotKey scrotKey
  where
    addScrotKey key x = x `additionalKeys` [ (first (.|. shiftMask) $ key, spawn "cd ~/tmp/scrot ; scrot '%Y-%m-%d-%H-%M-%s.png' -u -e 'feh --start-at ~/tmp/scrot/$f ~/tmp/scrot'")
                                           , (key                        , spawn "cd ~/tmp/scrot ; scrot '%Y-%m-%d-%H-%M-%s.png' -m -e 'feh --start-at ~/tmp/scrot/$f ~/tmp/scrot'") ]
