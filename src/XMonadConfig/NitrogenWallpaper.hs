module XMonadConfig.NitrogenWallpaper (setRandomNitrogenWallpaper) where

import Control.Monad (filterM)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (readFile)
import System.Process (callProcess)
import System.Random.Shuffle (shuffleM)

setRandomNitrogenWallpaper :: IO ()
setRandomNitrogenWallpaper = do
  home <- getEnv "HOME"
  let configPath = home </> ".config/nitrogen/nitrogen.cfg"
  configContent <- readFile configPath
  let dirsLine = case [drop 5 line | line <- lines configContent, "dirs=" `isPrefixOf` line] of
        (x : _) -> x
        [] -> ""
      dirsStr = filter (/= ';') dirsLine
      dirs = words dirsStr -- split by whitespace; if multiple dirs separated by spaces
  existingDirs <- filterM doesDirectoryExist dirs
  files <- fmap concat $ mapM listFilesRecursively existingDirs

  shuffled <- shuffleM files
  case shuffled of
    (file : _) -> callProcess "nitrogen" ["--set-scaled", file, "--save"]
    _ -> putStrLn "No image files found."

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively dir = do
  contents <- listDirectory dir
  paths <-
    mapM
      ( \p -> do
          let fullPath = dir </> p
          isDir <- doesDirectoryExist fullPath
          if isDir then listFilesRecursively fullPath else return [fullPath]
      )
      contents
  return $ concat paths
