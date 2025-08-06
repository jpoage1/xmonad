module XMonadConfig.PromptUtils (myXPConfig) where

import XMonad.Prompt

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
