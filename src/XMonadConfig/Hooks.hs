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
import XMonadConfig.Scratchpads (myScratchPads)
import XMonadConfig.StatusBar (launchPolybars, launchTaffybars)
import qualified XMonad.StackSet as W
import Data.Monoid
import XMonad.Util.NamedScratchpad
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser

myEditor :: String
-- myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor
myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor


myStartupHook :: X ()
myStartupHook = do
  io $ logToTmpFile "Startup hook triggered"
--   spawn "picom --backend glx" -- Compositor
  spawn "dunst"
  spawn "lxqt-session -w xmonad"
  spawn "lxqt-powermanagement"
  launchTaffybars
  spawn "setxkbmap -layout us -variant dvorak"
--   spawn "xss-lock --transfer-sleep-lock -- i3lock --nofork"
  -- launchPolybar

  startConkyIfEnabled
  io $ setRandomNitrogenWallpaper
  io $ logToTmpFile "Startup hook finished"

-- myManageHook :: ManageHook
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "Xmessage" --> doFloat,
    className =? "Zenity" --> doFloat,
    className =? "Conky" --> conkyHook,
    className =? "Xmessage" --> doFloat,
    isDialog --> doFloat
--   , title =? "conky_top"    --> myDoSink
--   , title =? "conky_middle" --> myDoSink
--   , title =? "conky_bottom" --> myDoSink
  , className =? "confirm"         --> doFloat
  , className =? "file_progress"   --> doFloat
  , className =? "dialog"          --> doFloat
  , className =? "download"        --> doFloat
  , className =? "error"           --> doFloat
  , className =? "Gimp"            --> doFloat
  , className =? "notification"    --> doFloat
  , className =? "pinentry-gtk-2"  --> doFloat
  , className =? "splash"          --> doFloat
  , className =? "toolbar"         --> doFloat
  , className =? "pavucontrol"         --> doFloat
  , className =? "pwvucontrol"         --> doFloat

  , className =? ".blueman-manager-wrapped" --> doFloat
  , className =? "nm-connection-editor" --> doFloat
  , className =? "Nm-connection-editor" --> doFloat
  , className =? "Yad"             --> doCenterFloat
  , title =? "Oracle VM VirtualBox Manager"   --> doFloat
--   , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
--   , className =? "Brave-browser"   --> doShift ( myWorkspaces !! 1 )
--   , className =? "mpv"             --> doShift ( myWorkspaces !! 7 )
--   , className =? "Gimp"            --> doShift ( myWorkspaces !! 8 )
--   , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , isFullscreen -->  doFullFloat
  ] <+> namedScratchpadManageHook myScratchPads

conkyHook :: Query (Endo WindowSet)
conkyHook = ask >>= \w -> liftX (withDisplay $ \d -> io $ lowerWindow d w) >> idHook
