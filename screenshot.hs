{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (SomeException, try)
import System.Exit (ExitCode (..))
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  result <- try $ readProcessWithExitCode "scrot" ["-s", "/tmp/screenshot.png"] ""
  case result of
    Left (e :: SomeException) ->
      putStrLn $ "Screenshot error: " ++ show e
    Right (exitCode, _, stderr) -> case exitCode of
      ExitSuccess -> do
        putStrLn "Screenshot taken. Enter filename (without .png):"
        hFlush stdout
        filename <- getLine
        if null filename
          then putStrLn "No filename entered. Screenshot saved to /tmp/screenshot.png"
          else do
            let dest = "/home/youruser/Pictures/" ++ filename ++ ".png"
            _ <- readProcessWithExitCode "mv" ["/tmp/screenshot.png", dest] ""
            putStrLn $ "Screenshot saved as " ++ dest
      _ -> putStrLn $ "scrot failed: " ++ stderr
