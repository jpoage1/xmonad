module XMonadConfig.Conky (runConky, startConkyIfEnabled, stopConky, raiseConkys, lowerConkys) where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (isJust)
import Data.Time
import Data.Time.Clock (getCurrentTime)
import System.Directory
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath
import System.FilePath (takeBaseName, (</>))
import System.IO (appendFile)
import System.Posix.Process (getProcessGroupIDOf, getProcessStatus)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process
import System.Process (spawnCommand)
import Text.Read (readMaybe)
import XMonad
import qualified XMonad.Util.Run as Run
import XMonadConfig.Logging (logToTmpFile)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Actions.WindowBringer (bringWindow)
-- import XMonad.X11 (getWindowAttributes, wa_override_redirect)
import Data.Maybe (isJust)
import Data.List (find, partition)
import Control.Monad (filterM)

runConky :: X ()
runConky = do
  io $ logToTmpFile "runConky triggered"
  home <- io getHomeDirectory
  io $ logToTmpFile $ "Home directory: " ++ home
  now <- io getCurrentTime
  let configDir = home </> ".config/conky/conf-enabled/"
      debugLog = home </> ".config/xmonad/conky_debug.log"
  files <- io $ listDirectory configDir
  io $ logToTmpFile $ "Files: " ++ show files
  let configs = map (configDir </>) files
  io $ logToTmpFile $ "Configs: " ++ show configs
  mapM_ (\config -> startConky config) configs

isConkyWithConfig :: FilePath -> Int -> IO Bool
isConkyWithConfig config pid = do
  let path = "/proc" </> show pid </> "cmdline"
  exists <- doesFileExist path
  if not exists
    then return False
    else do
      content <- BS.readFile path
      let args = BS8.split '\0' content
      return $
        any ((BS8.pack "conky" `BS8.isPrefixOf`) . BS8.strip) args
          && any (BS8.pack config `BS8.isInfixOf`) args

isConkyRunning :: FilePath -> IO Bool
isConkyRunning config = do
  logToTmpFile $ "Checking if conky is running with config: " ++ config
  pids <- listDirectory "/proc"
  logToTmpFile $ "Found entries in /proc: " ++ show pids
  let onlyPids = filter (all (`elem` ['0' .. '9'])) pids
  logToTmpFile $ "Filtered numeric PIDs: " ++ show onlyPids
  result <- anyM (\pid -> do
                     let pidInt = read pid
                     logToTmpFile $ "Checking PID: " ++ show pidInt
                     isConkyWithConfig config pidInt
                 ) onlyPids
  logToTmpFile $ "Final result: " ++ show result
  return result

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM p = foldM (\acc x -> if acc then return True else p x) False

startConkyIfNotRunning :: FilePath -> X ()
startConkyIfNotRunning config = do
  let baseName = takeBaseName config
      logDir = "~/.cache/conky"
      logPath = logDir </> ("conky-" ++ baseName ++ ".log")
  io $ do
    createDirectoryIfMissing True "/home/me/.cache/conky" -- expand manually
    logToTmpFile $ "Attempting to start conky: " ++ config
  running <- io $ isConkyRunning config
  if running
    then io $ logToTmpFile $ ": Conky already running with config: " ++ config
    else do
      io $ logToTmpFile $ ": Starting conky with config: " ++ config
      spawn $ "conky -c " ++ config ++ " >> /home/me/.cache/conky/conky-" ++ baseName ++ ".log 2>&1"

startConkyIfNotRunningPath :: FilePath -> X ()
startConkyIfNotRunningPath config = do
  let baseName = takeBaseName config
  io $ createDirectoryIfMissing True "/tmp/conky"
  running <- io $ isConkyRunning config
  io $ logToTmpFile ("Starting conky " ++ baseName)
  unless running $
    spawn $
      "conky -c " ++ config ++ " >> /tmp/conky/conky-" ++ baseName ++ ".log 2>&1"

startConky :: FilePath -> X ()
startConky config = do
  let baseName = takeBaseName config
  enabled <- io $ isConkyEnabled baseName
  if enabled
    then startConkyIfNotRunning config
    else return ()

isConkyEnabled :: String -> IO Bool
isConkyEnabled baseName = do
  mRunDir <- lookupEnv "XDG_RUNTIME_DIR"
  case mRunDir of
    Nothing -> return False
    Just runDir -> do
      let flagPath = runDir </> "conky-enabled" </> (baseName ++ ".enabled")
      doesFileExist flagPath

enableConkyConfig :: String -> IO ()
enableConkyConfig baseName = do
  mRunDir <- lookupEnv "XDG_RUNTIME_DIR"
  case mRunDir of
    Nothing -> return ()
    Just runDir -> do
      let dir = runDir </> "conky-enabled"
      createDirectoryIfMissing True dir
      writeFile (dir </> (baseName ++ ".enabled")) ""

disableConkyConfig :: String -> IO ()
disableConkyConfig baseName = do
  mRunDir <- lookupEnv "XDG_RUNTIME_DIR"
  case mRunDir of
    Nothing -> return ()
    Just runDir -> do
      let path = runDir </> "conky-enabled" </> (baseName ++ ".enabled")
      exists <- doesFileExist path
      when exists $ removeFile path

startConkyIfEnabled :: X ()
startConkyIfEnabled = do
  enabled <- io listEnabledConfigs
  io $ logToTmpFile $ "[startConkyIfEnabled] Enabled config names: " ++ show enabled

  let confDir = "/home/me/.config/conky/conf-enabled"
  io $ logToTmpFile $ "[startConkyIfEnabled] Scanning directory: " ++ confDir

  confFiles <- io $ listDirectory confDir
  io $ logToTmpFile $ "[startConkyIfEnabled] Files in conf-enabled: " ++ show confFiles

  let matchingConfs =
        [ confDir </> f
        | f <- confFiles
        , takeBaseName f `elem` enabled
        , ".conf" `isSuffixOf` f
        ]

  io $ logToTmpFile $ "[startConkyIfEnabled] Matching configs to start: " ++ show matchingConfs

  mapM_ (\path -> do
            io $ logToTmpFile $ "[startConkyIfEnabled] Attempting to start: " ++ path
            startConkyIfNotRunningPath path
        ) matchingConfs

stopConky :: X ()
stopConky = do
  enabled <- io listEnabledConfigs
  mapM_
    ( \base -> do
        let config = "/home/me/.config/conky/conf-enabled" </> (base ++ ".conf")
        stopConkyWithConfig config
    )
    enabled

listEnabledConfigs :: IO [String]
listEnabledConfigs = do
  logToTmpFile "Starting listEnabledConfigs"
  mRunDir <- lookupEnv "XDG_RUNTIME_DIR"
  logToTmpFile $ "XDG_RUNTIME_DIR lookup result: " ++ show mRunDir
  case mRunDir of
    Nothing -> do
      logToTmpFile "No XDG_RUNTIME_DIR found, returning empty list"
      return []
    Just runDir -> do
      let dir = runDir </> "conky-enabled"
      logToTmpFile $ "Looking for directory: " ++ dir
      exists <- doesDirectoryExist dir
      logToTmpFile $ "Directory exists? " ++ show exists
      if not exists
        then do
          logToTmpFile "Directory missing, returning empty list"
          return []
        else do
          files <- listDirectory dir
          logToTmpFile $ "Files found: " ++ show files
          let enabled = [takeBaseName f | f <- files, ".enabled" `isSuffixOf` f]
          logToTmpFile $ "Enabled configs: " ++ show enabled
          return enabled

stopConkyWithConfig :: FilePath -> X ()
stopConkyWithConfig config = do
  pids <- io $ listDirectory "/proc"
  let onlyPids = filter (all (`elem` ['0' .. '9'])) pids
  io $ forM_ onlyPids $ \pidStr -> do
    let pid = read pidStr :: Int
    valid <- isConkyWithConfig config pid
    when valid $ (try (signalProcess nullSignal (fromIntegral pid)) :: IO (Either SomeException ())) >> return ()

conkyTitles :: [String]
conkyTitles = ["conky_bottom", "conky_middle", "conky_top"]

isConky :: Window -> X Bool
isConky w = do
  name <- getName w
  return (show name `elem` conkyTitles)

getConkys :: X [Window]
getConkys = withWindowSet $ \ws -> filterM isConky (W.integrate' . W.stack . W.workspace . W.current $ ws)

raiseConkys :: X ()
raiseConkys = do
  runConky
  wins <- getConkys
  mapM_ (\w -> windows (W.shiftMaster . W.focusWindow w)) (reverse wins)

-- lowerConkys :: X ()
-- lowerConkys = do
--   wins <- getConkys
--   mapM_ (\w -> windows (W.sink w)) wins
--   ws <- gets windowset
--   let allWins = W.integrate' . W.stack . W.workspace . W.current $ ws
--       (cons, rest) = partition (`elem` wins) allWins
--   windows $ \s -> s { W.current = (W.current s)
--     { W.workspace = (W.workspace (W.current s))
--       { W.stack = Just $ W.Stack (head rest) [] (tail rest ++ cons) } } }

lowerConkys :: X ()
lowerConkys = do
  runConky
  conkyWins <- getConkys
  -- Sink all conky windows first
  mapM_ (\w -> windows (W.sink w)) conkyWins
  -- Then send each to the bottom of the stack
  mapM_ (\w -> windows (W.swapDown . W.focusWindow w)) conkyWins
