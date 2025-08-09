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
import XMonadConfig.Conky (runConky, stopConky, raiseConkys, lowerConkys)
import XMonadConfig.Logging (logToTmpFile)
import XMonadConfig.PromptUtils
import XMonadConfig.StatusBar
import XMonad.Hooks.SetWMName

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser


myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-'", kill),
    ("<Print>", unGrab *> spawn "scrot -s ~/Pictures/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    ("M-<Print>", takeScreenshotWithPrompt "/tmp/screenshot.png"),
    ("M-S-.", confirmPrompt myXPConfig "Exit XMonad?" $ io exitSuccess),
    ("M-l", spawn "i3lock"),
    ("M-c", stopConky),
    ("M-S-c", raiseConkys),
    ("M-C-c", lowerConkys),
    ("M-S-p", launchTaffybars),
    ("M-S-r", spawn "xmonad --restart"),
    ("M-m", spawn "rofi -show drun"),
    ("M-w", spawn "rofi -show window"),
    ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5"),
    ("<XF86AudioLowerVolume>", spawn "pamixer -d 5"),
    ("<XF86AudioMute>", spawn "pamixer -t"),
    ("M-<Insert>", spawn "toggle-kb-layout.sh"), -- Work-around
    ("M-S-<Insert>", toggleKBLayout), -- Not Working
    ("<Insert>", pasteSelection),
    ("M-S-h", withFocused (keysResizeWindow (-10, 0) (0, 0))),
    ("M-S-l", withFocused (keysResizeWindow (10, 0) (0, 0))),
    ("M-S-k", withFocused (keysResizeWindow (0, -10) (0, 0))),
    ("M-S-j", withFocused (keysResizeWindow (0, 10) (0, 0))),
    ("M-S-<Down>", withFocused (keysResizeWindow (0, 10) (0, 0))),
    ("M-S-<Up>", withFocused (keysResizeWindow (0, -10) (0, 0))),
    ("M-S-<Right>", withFocused (keysResizeWindow (10, 0) (0, 0))),
    ("M-S-<Left>", withFocused (keysResizeWindow (-10, 0) (0, 0))),
    ("M-b", sendMessage ToggleStruts)
  , ("M-<Return>", spawn (myTerminal))
  , ("M-b", spawn (myBrowser))
  , ("M-M1-h", spawn (myTerminal ++ " -e htop"))]



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
