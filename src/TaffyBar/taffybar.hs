{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig 
import System.Taffybar.Widget
import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.FSMonitor
import System.Taffybar.Widget.DiskIOMonitor
import System.Taffybar.Widget.Layout
import System.Taffybar.Widget.NetworkGraph
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.WttrIn
import System.Environment (getArgs)


cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]
myBarConfig :: SimpleTaffyConfig
myBarConfig =
  let cpuCfg =
        def
          { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
            graphLabel = Just "cpu"
          }
      clock = textClockNewWith def
      batteryWidget = batteryIconNew 
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      notifications = notifyAreaNew defaultNotificationConfig
      los = layoutNew def
      workspaces = workspacesNew def
  in def
      { startWidgets = [ los, workspaces ],
        endWidgets =
          [ clock
          , notifications
          , batteryWidget
          , cpu
          , sniTrayNew
          ]
      }

fullConfig = toTaffybarConfig $ myBarConfig

topBarConfig = def
  { barPosition = Top
  , startWidgets = startWidgets myBarConfig
  , endWidgets = endWidgets myBarConfig
  }

bottomBarConfig = def
  { barPosition = Bottom
  , startWidgets = startWidgets myBarConfig
  , endWidgets = endWidgets myBarConfig
  }

setPosition :: String -> Position
setPosition "Top"    = Top
setPosition "Bottom" = Bottom
setPosition _        = Top

-- main = startTaffybar $ toTaffybarConfig bottomBarConfig

main = do
  args <- getArgs
  config <- case args of
            ["top"]    -> return topBarConfig
            ["bottom"] -> return bottomBarConfig
            _          -> return topBarConfig

  startTaffybar $ toTaffybarConfig config
-- Polybar
-- Top left: RAM XX% | SWAP XX% | CPU XX%
-- Top right: systray | network device ip |KeyboardLayout | volume | battery | time

-- Bottom left: workspaces
-- Bottom right: Disk space
