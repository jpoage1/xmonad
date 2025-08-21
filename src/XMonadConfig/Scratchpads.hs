module XMonadConfig.Scratchpads (myScratchPads) where

import XMonad
import XMonadConfig.Logging (logToTmpFile)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser

myEditor :: String
-- myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor
myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor


myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "htop" spawnHtop findHtop manageHtop
                , NS "kweather" spawnKweather findKweather manageKweather
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-qt"
    findCalc   = className =? "qalculate-qt"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
    spawnHtop = myTerminal ++ " -t scratchpad-htop -e htop"
    findHtop  = title =? "scratchpad-htop"
    manageHtop = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 - h
                 l = 0.95 - w
    spawnKweather = "kweather"
    findKweather  = className =? "kweather"
    manageKweather = customFloating $ W.RationalRect l t w h
                  where h = 0.9; w = 0.9; t = 0.95 - h; l = 0.95 - w
