module XMonadConfig.Hooks (myStartupHook, myManageHook) where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BorderResize
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonadConfig.Conky (startConkyIfEnabled)
import XMonadConfig.Logging (logToTmpFile)
import XMonadConfig.NitrogenWallpaper (setRandomNitrogenWallpaper)
import XMonadConfig.StatusBar (launchPolybars, launchTaffybars)

myStartupHook :: X ()
myStartupHook = do
  io $ logToTmpFile "Startup hook triggered"
  spawn "picom --backend glx"
  launchTaffybars
  spawn "setxkbmap -layout us -variant dvorak"
  spawn "dunst"
  spawn "xss-lock --transfer-sleep-lock -- i3lock --nofork"
  -- launchPolybar
  io $ setRandomNitrogenWallpaper
  io $ logToTmpFile "Startup hook finished"
  startConkyIfEnabled

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Xmessage" --> doFloat,
      className =? "Zenity" --> doFloat,
      className =? "Conky" --> doFloat,
      className =? "Xmessage" --> doFloat,
      isDialog --> doFloat
    ]
