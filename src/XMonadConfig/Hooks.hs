module XMonadConfig.Hooks (myStartupHook, myManageHook) where

import XMonad
import XMonad.Actions.MouseResize
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
import XMonadConfig.StatusBar (launchTaffybars)
import qualified XMonad.StackSet as W
import Data.Monoid
import XMonad.Util.NamedScratchpad
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Control.Concurrent (forkIO, threadDelay)
myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "qutebrowser "

myEditor :: String
myEditor = myTerminal ++ " -e vim "

myStartupHook :: X ()
myStartupHook = do
    io $ logToTmpFile "Startup hook triggered"

    -- lightweight spawns
    mapM_ spawn [
        -- "picom --backend glx",
        -- "dunst",
        -- "xss-lock --transfer-sleep-lock -- i3lock --nofork",
        "lxqt-powermanagement",
        "setxkbmap -layout us -variant dvorak"]

    -- fork IO-heavy or delayed tasks
    io $ forkIO $ do
        launchTaffybars
        setRandomNitrogenWallpaper
        return ()

    spawn "lxqt-session -w xmonad"
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
