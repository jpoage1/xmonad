{-# LANGUAGE LambdaCase #-}

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Graphics.X11.ExtraTypes.XF86
import NitrogenWallpaper (setRandomNitrogenWallpaper)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Exit (ExitCode (..), exitSuccess)
import System.FilePath ((</>))
import System.IO (getLine, hFlush, hGetContents, putStr, stdout)
import System.Process
import System.Process (CreateProcess (..), StdStream (..), callProcess, createProcess, proc, waitForProcess)
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Operations (unGrab)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Input (inputPrompt)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.SpawnOnce

-- import XMonad.Operations.unGrab

main :: IO ()
main =
  xmonad $
    ewmhFullscreen $
      ewmh
        def
          { modMask = mod4Mask, -- Rebind Mod to the Super key
            layoutHook = avoidStruts $ myLayout, -- Use custom layouts
            manageHook = manageDocks <+> myManageHook <+> manageHook def,
            terminal = "alacritty",
            focusFollowsMouse = False,
            startupHook = myStartupHook
          }
        `additionalKeysP` [ ("M-S-'", kill),
                            ("<Print>", unGrab *> spawn "scrot -s ~/Pictures/screenshot_%Y-%m-%d_%H-%M-%S.png"),
                            ("M-<Print>", takeScreenshotWithPrompt "/tmp/screenshot.png"),
                            ("M-S-.", confirmPrompt myXPConfig "Exit XMonad?" $ io exitSuccess),
                            ("M-S-l", spawn "i3lock"),
                            ("M-S-p", launchPolybar),
                            -- ("M-S-m", spawn "alacritty workspaces"), -- Rofi Menu
                            ("M-m", spawn "rofi -show drun"), -- Show applications
                            ("M-w", spawn "rofi -show window"), -- Show open windows
                            ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5"),
                            ("<XF86AudioLowerVolume>", spawn "pamixer -d 5"),
                            ("<XF86AudioMute>", spawn "pamixer -t"),
                            ("M-<Insert>", toggleKBLayout)
                          ]

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

launchBar :: String -> X ()
launchBar bar = spawn $ "polybar " ++ bar ++ " 2>&1 | tee -a /tmp/polybar-" ++ bar ++ ".log"

launchPolybar :: X ()
launchPolybar = do
  spawn "killall -q polybar"
  launchBar "xmonad-top"
  launchBar "xmonad-bottom"

myStartupHook :: X ()
myStartupHook = do
  spawn "setxkbmap -layout us -variant dvorak"
  spawn "dunst"
  spawn "xss-lock --transfer-sleep-lock -- i3lock --nofork"
  launchPolybar
  io setRandomNitrogenWallpaper

toggleKBLayout :: X ()
toggleKBLayout = do
  layout <- io $ readProcess "setxkbmap" ["-query"] ""
  let isDvorak = any ("dvorak" `isInfixOf`) (lines layout)
  if isDvorak
    then spawn "setxkbmap us"
    else spawn "setxkbmap us -variant dvorak"

myManageHook =
  composeAll
    [ className =? "Xmessage" --> doFloat,
      className =? "Zenity" --> doFloat
      -- add other rules here
    ]

exitWithConfirm :: X ()
exitWithConfirm = confirmPrompt def "Exit XMonad?" $ io exitSuccess


-- takeScreenshot function, receives tmp path and IO String action for final filename
takeScreenshot :: FilePath -> IO FilePath -> IO ()
takeScreenshot tmpPath getFilename = do
  result <- try $ callProcess "scrot" ["-s", tmpPath] :: IO (Either SomeException ())
  case result of
    Left _ -> return ()
    Right _ -> do
      finalFile <- getFilename
      _ <- try $ renameFile tmpPath finalFile :: IO (Either SomeException ())
      return ()


takeScreenshotWithPrompt :: FilePath -> X ()
takeScreenshotWithPrompt tmpPath = do
  -- Take screenshot first
  liftIO $ callProcess "scrot" ["-s", tmpPath]
  -- Then get filename from user
  filename <- getInputString
  -- Rename file to final filename
  liftIO $ renameFile tmpPath filename


getInputString :: X String
getInputString = do
  mbInput <- inputPrompt myXPConfig "Enter filename (including extension): "
  return $ maybe "" id mbInput


myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:Ubuntu Mono:size=12",
      bgColor = "#282A2E",
      fgColor = "#C5C8C6",
      bgHLight = "#F0C674",
      fgHLight = "#282A2E",
      borderColor = "#F0C674",
      promptBorderWidth = 2,
      position = CenteredAt 0.5 0.5,
      height = 50
    }
