{-# LANGUAGE LambdaCase #-}

import System.Process (callProcess)
import System.Directory (renameFile)
import Control.Exception (try, SomeException)

import System.IO (hFlush, stdout)
import System.IO (putStr, getLine)
import XMonad

import qualified Data.Text as T


-- Main uses helper and prompt, no logic duplication
main :: IO ()
main = do
  let tmpPath = "/tmp/screenshot.png"
  takeScreenshot tmpPath getFilenamePrompt

-- Usage in main or X monad, no console I/O inside helper
saveScreenshotWithPrompt :: X ()
saveScreenshotWithPrompt = do
  let tmpPath = "/tmp/screenshot.png"
  liftIO $ takeScreenshot tmpPath $ do
    -- This IO action is a lambda returning the final filename string
    fmap T.unpack <$> inputPrompt myXPConfig "Enter filename (including extension): "
    >>= \case
      Nothing -> return (tmpPath ++ ".discard")  -- or delete later
      Just "" -> return ("/home/user/Pictures/screenshot-" ++ dateStamp ++ ".png")
      Just name -> return ("/home/user/Pictures/" ++ name ++ ".png")
  where
    dateStamp = "$(date +%Y%m%d-%H%M%S)" -- ideally resolved shell-side or by Haskell

-- | Take a screenshot with scrot, then move it to a filename from the lambda.
--   No output returned; exceptions are caught silently.
takeScreenshot :: FilePath      -- ^ Temporary screenshot path
               -> IO FilePath  -- ^ Lambda returning final filename (including extension)
               -> IO ()

takeScreenshot tmpPath getFilename = do
  result <- try $ callProcess "scrot" ["-s", tmpPath] :: IO (Either SomeException ())
  case result of
    Left _ -> return ()  -- ignore errors silently; can log or extend as needed
    Right _ -> do
      finalFile <- getFilename
      _ <- try $ renameFile tmpPath finalFile :: IO (Either SomeException ())
      return ()
