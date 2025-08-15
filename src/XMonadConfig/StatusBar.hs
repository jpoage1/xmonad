module XMonadConfig.StatusBar (launchPolybars, launchTaffybars) where

import XMonad
import XMonadConfig.Logging (logToTmpFile)

launchPolybar :: String -> X ()
launchPolybar bar = spawn $ "polybar " ++ bar ++ " 2>&1 | tee -a /tmp/polybar-" ++ bar ++ ".log"

launchPolybars :: X ()
launchPolybars = do
  spawn "killall -q polybar"
  launchPolybar "xmonad-top"
  launchPolybar "xmonad-bottom"

launchTaffybar :: String -> X ()
launchTaffybar bar = do
  liftIO $ logToTmpFile $ "Spawning my-taffybar " ++ bar
  liftIO $ logToTmpFile $ "my-taffybar " ++ bar ++ " 2>&1 | tee -a /tmp/my-taffybar-" ++ bar ++ ".out"
  spawn $ "my-taffybar " ++ bar ++ " 2>&1 | tee -a /tmp/my-taffybar-" ++ bar ++ ".out"

-- launchTaffybars :: X ()
-- launchTaffybars = do
--   liftIO $ logToTmpFile $ "Stopping any running taffybar instances..."
--   spawn "pkill my-taffybar"
--   spawn $ "sleep 2"
--   launchTaffybar "top"
--   launchTaffybar "bottom"


-- launchTaffybars :: X ()
-- launchTaffybars = spawn "~/.config/xmonad/statusbar.sh"

launchTaffybars :: X ()
launchTaffybars = do
  liftIO $ logToTmpFile $ "Stopping any running taffybar instances..."
  spawn $ unwords
    [ "pkill my-taffybar;"
    , "sleep 1;"
    , "my-taffybar top 2>&1 | tee -a /tmp/my-taffybar-top.out &"
    , "my-taffybar bottom 2>&1 | tee -a /tmp/my-taffybar-bottom.out &"
    ]
