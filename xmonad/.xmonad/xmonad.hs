{-# LANGUAGE LambdaCase #-}

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Spacing
import           XMonad.Layout.Grid

featureColor :: String
featureColor = "#ffd972"

urgentColor :: String
urgentColor = "#ef3e36"

dimColor :: String
dimColor = "#777"

gapSize :: Integer
gapSize = 5

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP (const (mod1Mask, xK_b)) def
  { terminal = "kitty"
  , modMask = mod1Mask
  , borderWidth = 8
  , focusedBorderColor = featureColor
  , workspaces = ["www", "dev", "talk", "etc"]
  , layoutHook = avoidStruts
      ( tall ||| Full ||| gaps tall ||| gaps grid )
  }

equidistant :: Integer -> Border
equidistant n = Border n n n n

gaps = spacingRaw True border True border True
  where border = equidistant gapSize

tall :: Tall a
tall = Tall 1 (3/100) (1/2)

grid :: Grid a
grid = GridRatio (3/3)

myPP :: PP
myPP = def
  { ppCurrent = xmobarColor featureColor ""
  , ppUrgent = xmobarColor urgentColor ""
  , ppLayout = \case
      "Tall"          -> "[│]"
      "Full"          -> "[ ]"
      "Spacing Tall"  -> "[║]"
      "Spacing GridRatio 1.0" -> "[#]"
  , ppSep = xmobarColor dimColor "" " ∙ "
  , ppTitle = xmobarColor dimColor ""
  , ppHiddenNoWindows = id
  }
