module XMonadConfig.Main where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.BorderResize
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig (additionalKeysP)
import XMonadConfig.Conky (runConky)
import XMonadConfig.Hooks
import XMonadConfig.Keys
import XMonadConfig.Logging (logToTmpFile)
import XMonadConfig.NitrogenWallpaper (setRandomNitrogenWallpaper)
import XMonadConfig.Polybar (launchPolybar)

main :: IO ()
main =
  do
    logToTmpFile "Xmonad starting..."
    xmonad $
      withSB mySB $
        ewmhFullscreen $
          ewmh $
            pagerHints $
              docks
                def
                  { modMask = mod4Mask,
                    layoutHook = myLayout,
                    manageHook = manageDocks <+> myManageHook <+> manageHook def,
                    terminal = "alacritty",
                    focusFollowsMouse = False,
                    startupHook = myStartupHook
                  }
                `additionalKeysP` myKeys
    logToTmpFile "Xmonad ready to go"

myLayout = avoidStruts $ mouseResize $ borderResize $ smartBorders $ tiled ||| Mirror tiled ||| Full ||| threeCol ||| simpleFloat
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

mySB = statusBarProp "xmobar" (pure xmobarPP)
