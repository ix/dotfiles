{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

import           XMonad
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Spacing

import qualified Data.Map as M

featureColor :: String
featureColor = "#FC4E71"

urgentColor :: String
urgentColor = "#ef3e36"

dimColor :: String
dimColor = "#777"

dimmerColor :: String
dimmerColor = "#444"

gapSize :: Integer
gapSize = 5

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP (const (mod1Mask, xK_b)) def
  { terminal = "kitty"
  , modMask = mod4Mask
  , borderWidth = 8
  , focusedBorderColor = featureColor
  , workspaces = ["i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"]
  , layoutHook = avoidStruts
      ( tall ||| Full ||| gaps tall ||| gaps grid )
  , keys = myKeymap
  }

myKeymap :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeymap layout = M.fromList keybinds <> keys def layout
  where keybinds =
          [
            ((mod4Mask .|. shiftMask, xK_s), spawn "shootregion")
          ]

equidistant :: Integer -> Border
equidistant n = Border n n n n

gaps :: LayoutClass l a => l a -> ModifiedLayout Spacing l a
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
  , ppHidden = id
  , ppHiddenNoWindows = xmobarColor dimColor ""
  }
