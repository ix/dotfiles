{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE UnicodeSyntax #-}

import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import Colors

import qualified Data.Map as M

featureColor :: String
featureColor = color1

urgentColor :: String
urgentColor = color5

dimColor :: String
dimColor = color8

dimmerColor :: String
dimmerColor = color9

gapSize :: Integer
gapSize = 5

superGapSize :: Integer
superGapSize = 20

modifier :: KeyMask
modifier = mod4Mask

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP (const (mod1Mask, xK_b)) def
  { terminal = "urxvt"
  , modMask = mod4Mask
  , borderWidth = 4
  , focusedBorderColor = featureColor
  , workspaces = ["i", "ii", "iii", "iv"]
  , layoutHook = avoidStruts
      ( tall ||| Full ||| gaps tall ||| gaps (Mirror tall) ||| gaps grid ||| supergaps grid )
  , keys = myKeymap
  }

dmenu :: String
dmenu = mconcat
  [ "dmenu_run -fn \"Misc Terminusmodx:size=9\""
  , " -sb " ++ mconcat ["\"", featureColor, "\""]
  , " -nb " ++ mconcat ["\"", background, "\""]
  , " -nf " ++ mconcat ["\"", foreground, "\""]
  , " -sf " ++ mconcat ["\"", background, "\""]
  ]

myKeymap :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeymap layout = M.fromList keybinds <> keys def layout
  where keybinds =
          [
            ((modifier .|. shiftMask, xK_s), spawn "shootregion")
          , ((modifier .|. shiftMask, xK_p), spawn "snatch")
          , ((modifier .|. shiftMask, xK_e), spawn "emacsclient --create-frame")
          , ((modifier, xK_p), spawn dmenu)
          , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
          , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
          ]

equidistant :: Integer -> Border
equidistant n = Border n n n n

gaps :: LayoutClass l a => l a -> ModifiedLayout Spacing l a
gaps = spacingRaw True border True border True
  where border = equidistant gapSize

supergaps :: LayoutClass l a => l a -> ModifiedLayout Spacing l a
supergaps = spacingRaw True border True border True
  where border = equidistant superGapSize

tall :: Tall a
tall = Tall 1 (3/100) (1/2)

grid :: Grid a
grid = GridRatio (3/3)

myPP :: PP
myPP = def
  { ppCurrent = const $ xmobarColor featureColor "" "\x00e5"
  , ppUrgent = const $ xmobarColor urgentColor "" "\x00e5"
  , ppLayout = \case
      "Tall"          -> "\x00fe"
      "Full"          -> "\x00ff"
      "Spacing Tall"  -> "\x00f6"
      "Spacing Mirror Tall" -> "\x00fc"
      "Spacing GridRatio 1.0" -> "\x00fa"
  , ppSep = xmobarColor dimColor "" " / "
  , ppTitle = xmobarColor dimColor ""
  , ppHidden = const $ "\x00e5"
  , ppHiddenNoWindows = const $ xmobarColor dimColor "" "\x00e5"
  }
