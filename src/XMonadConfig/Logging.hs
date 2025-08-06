module XMonadConfig.Logging (logToTmpFile) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getZonedTime)
import System.IO
import XMonad

logToTmpFile :: String -> IO ()
logToTmpFile msg = io $ do
  let path = "/tmp/xmonad-debug.log"
  now <- getZonedTime
  let timestamped = formatTime defaultTimeLocale "%F %T%z" now ++ " - " ++ msg ++ "\n"
  appendFile path timestamped
