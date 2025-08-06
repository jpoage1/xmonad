module XMonadConfig.Conky (runConky, startConkyIfEnabled, stopConky) where

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
  pids <- listDirectory "/proc"
  let onlyPids = filter (all (`elem` ['0' .. '9'])) pids
  anyM (isConkyWithConfig config . read) onlyPids

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
  mapM_
    ( \base -> do
        let config = "/home/me/.config/conky/conf-enabled" </> (base ++ ".conf")
        startConkyIfNotRunningPath config
    )
    enabled

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
  mRunDir <- lookupEnv "XDG_RUNTIME_DIR"
  case mRunDir of
    Nothing -> return []
    Just runDir -> do
      let dir = runDir </> "conky-enabled"
      exists <- doesDirectoryExist dir
      if not exists
        then return []
        else do
          files <- listDirectory dir
          return [takeBaseName f | f <- files, ".enabled" `isSuffixOf` f]

startConkyIfNotRunningPath :: FilePath -> X ()
startConkyIfNotRunningPath config = do
  let baseName = takeBaseName config
  io $ createDirectoryIfMissing True "/home/me/.cache/conky"
  running <- io $ isConkyRunning config
  unless running $
    spawn $
      "conky -c " ++ config ++ " >> /home/me/.cache/conky/conky-" ++ baseName ++ ".log 2>&1"

stopConkyWithConfig :: FilePath -> X ()
stopConkyWithConfig config = do
  pids <- io $ listDirectory "/proc"
  let onlyPids = filter (all (`elem` ['0' .. '9'])) pids
  io $ forM_ onlyPids $ \pidStr -> do
    let pid = read pidStr :: Int
    valid <- isConkyWithConfig config pid
    when valid $ (try (signalProcess nullSignal (fromIntegral pid)) :: IO (Either SomeException ())) >> return ()
