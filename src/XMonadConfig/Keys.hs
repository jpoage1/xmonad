module XMonadConfig.Keys (myKeys) where

import Data.List (isInfixOf)
import System.Directory (renameFile)
import System.Exit (exitSuccess)
import System.Process (callProcess, readProcess)
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Operations (unGrab)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Input
import XMonad.Util.Paste
import XMonad.Util.Run (safeSpawn)
import XMonadConfig.Conky (runConky, stopConky, raiseConky, lowerConkys)
import XMonadConfig.Logging (logToTmpFile)
import XMonadConfig.PromptUtils
import XMonadConfig.StatusBar
import XMonad.Hooks.SetWMName
import System.Process (readProcess, callCommand)

import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActions
import XMonad.Hooks.SetWMName
import XMonad.Util.NamedActions
import XMonadConfig.Scratchpads (myScratchPads)




myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser


myKeys :: [(String, X ())]
myKeys =
  [
    -- ("<XF86AudioRaiseVolume>", io $ changeVolume "-i 5"),
    -- ("<XF86AudioLowerVolume>", io $ changeVolume "-d 5"),
    -- ("<XF86AudioMute>",       io $ changeVolume "-t"),
    ("<XF86AudioRaiseVolume>", spawn "pamixer --allow-boost -i 5")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
  , ("<XF86AudioMute>", spawn "pamixer -t")
  , ("<Insert>", pasteSelection)

  , ("M-S-w", windows $ W.swapMaster . W.focusDown)
  , ("M-S-s", windows $ W.shiftMaster . W.focusDown)
  , ("M-C-w", windows $ W.swapMaster . W.focusUp)
  , ("M1-<Tab>", windows $ W.shiftMaster . W.focusUp)
  , ("M1-S-<Tab>", windows W.focusDown)

    -- Conky
  , ("M-c", stopConky)
--   , ("M-S-c", raiseConkys)
  , ("M-S-c", raiseConky)
  , ("M-C-c", lowerConkys)

  -- Screenshot
  , ("<Print>", unGrab *> spawn "scrot -s ~/Pictures/screenshot_%Y-%m-%d_%H-%M-%S.png")
  , ("M-<Print>", takeScreenshotWithPrompt "/tmp/screenshot.png")

    -- Super
  , ("M-b", sendMessage ToggleStruts)
  , ("M-<Return>", spawn (myTerminal))
  , ("M-b", spawn (myBrowser))
  , ("M-<Insert>", spawn "toggle-kb-layout.sh")
  , ("M-m", spawn "rofi -show drun")
  , ("M-w", spawn "rofi -show window")
  , ("M-l", spawn "i3lock")

    -- Super + Shift
  , ("M-S-'", kill)
  , ("M-S-<Insert>", toggleKBLayout) -- Not Working
  , ("M-S-h", withFocused (keysResizeWindow (-10, 0) (0, 0)))
  , ("M-S-l", withFocused (keysResizeWindow (10, 0) (0, 0)))
  , ("M-S-k", withFocused (keysResizeWindow (0, -10) (0, 0)))
  , ("M-S-j", withFocused (keysResizeWindow (0, 10) (0, 0)))
  , ("M-S-<Down>", withFocused (keysResizeWindow (0, 10) (0, 0)))
  , ("M-S-<Up>", withFocused (keysResizeWindow (0, -10) (0, 0)))
  , ("M-S-<Right>", withFocused (keysResizeWindow (10, 0) (0, 0)))
  , ("M-S-<Left>", withFocused (keysResizeWindow (-10, 0) (0, 0)))
  , ("M-S-p", io $ launchTaffybars)
  , ("M-S-r", spawn "xmonad --restart")
  , ("M-S-.", confirmPrompt myXPConfig "Exit XMonad?" $ io exitSuccess)

  -- Super + Alt
  , ("M-M1-h", namedScratchpadAction myScratchPads "htop")
  , ("M-M1-t", namedScratchpadAction myScratchPads "terminal")
  , ("M-<Escape>", namedScratchpadAction myScratchPads "calculator")
  , ("M-M1-w", namedScratchpadAction myScratchPads "kweather")
  ]



toggleKBLayout :: X ()
toggleKBLayout = do
  io $ logToTmpFile "toggleKBLayout triggered"
  layout <- io $ readProcess "setxkbmap" ["-print"] ""
  io $ logToTmpFile $ "Layout print output: " ++ show layout

  let isDvorak = "dvorak" `isInfixOf` layout
  io $ logToTmpFile $ "Is Dvorak: " ++ show isDvorak

  if isDvorak
    then do
      io $ logToTmpFile "Switching to QWERTY"
      io $ callProcess "setxkbmap" ["us"]
    else do
      io $ logToTmpFile "Switching to Dvorak"
      io $ callProcess "setxkbmap" ["us", "-variant", "dvorak"]

takeScreenshotWithPrompt :: FilePath -> X ()
takeScreenshotWithPrompt tmpPath = do
  liftIO $ callProcess "scrot" ["-s", tmpPath]
  filename <- inputPrompt myXPConfig "Enter filename (including extension): "
  case filename of
    Nothing -> return ()
    Just fn -> liftIO $ renameFile tmpPath fn

changeVolume :: String -> IO ()
changeVolume volumeCmd = do
    let msgTag = "myvolume"
    -- Change the volume using pamixer
    callCommand $ "pamixer --allow-boost " ++ volumeCmd

    -- Get volume percentage
    volumeStr <- readProcess "pamixer" ["--get-volume"] ""
    let volume = filter (/= '\n') volumeStr

    io $ logToTmpFile $ "notify-send " ++ volume

    -- Get mute status
    muteStr <- readProcess "pamixer" ["--get-mute"] ""
    let mute = filter (/= '\n') muteStr
    -- Show notification
    if mute == "true" || volume == "0"
    then callProcess "dunstify"
        ["-a","changeVolume","-u","low","-i","audio-volume-muted","-h","string:x-dunst-stack-tag:" ++ msgTag,"Volume muted"]
    else callProcess "dunstify"
        ["-a","changeVolume","-u","low","-i","audio-volume-high","-h","string:x-dunst-stack-tag:" ++ msgTag,"-h","int:value:" ++ volume,"Volume: " ++ volume ++ "%"]

    -- Play sound
    callCommand "canberra-gtk-play -i audio-volume-change -d changeVolume"
