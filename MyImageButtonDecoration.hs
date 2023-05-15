{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- adapted from XMonad.Layout.ImageButtonDecoration
-- want larger text size and fewer buttons
-- also includes adaption of XMonad.Layout.Tabbed

module MyImageButtonDecoration
    ( imageButtonDeco
    , defaultThemeWithImageButtons
    , shrinkText
    , imageTitleBarButtonHandler
    , ImageButtonDecoration
    , imageButtonTabbed
    , TabbedImageButtonDecoration 
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Util.Image

import XMonad.Actions.WindowMenu
import XMonad.Actions.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Simplest

import qualified XMonad.StackSet as S

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (unless)

buttonSize :: Int
buttonSize = 20

menuButtonOffset :: Int
menuButtonOffset = 4

closeButtonOffset :: Int
closeButtonOffset = 8


-- The images in a 0-1 scale to make
-- it easier to visualize

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (== 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

menuButton' :: [[Int]]
menuButton' = [[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
               [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
               [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
               [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

closeButton' :: [[Int]]
closeButton' = [[1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1],
                [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
                [0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0],
                [0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0],
                [0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0],
                [0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0],
                [0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0],
                [0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
                [0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0],
                [0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0],
                [0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0],
                [0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0],
                [0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0],
                [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
                [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1]]

closeButton :: [[Bool]]
closeButton = convertToBool closeButton'

-- | A function intended to be plugged into the 'decorationCatchClicksHook' of a decoration.
-- It will intercept clicks on the buttons of the decoration and invoke the associated action.
-- To actually see the buttons, you will need to use a theme that includes them.
-- See 'defaultThemeWithImageButtons' below.
imageTitleBarButtonHandler :: Window -> Int -> Int -> X Bool
imageTitleBarButtonHandler mainw distFromLeft distFromRight = do
    let action
          -- | fi distFromLeft >= menuButtonOffset &&
          --    fi distFromLeft <= menuButtonOffset + buttonSize = focus mainw >> windowMenu >> return True
          | fi distFromRight >= closeButtonOffset &&
            fi distFromRight <= closeButtonOffset + buttonSize = focus mainw >> kill >> return True
          | otherwise = return False
    action

defaultThemeWithImageButtons :: Theme
defaultThemeWithImageButtons = def
    { windowTitleIcons =
        [ -- (menuButton, CenterLeft 3),
          (closeButton, CenterRight 3)
        ]
    --, fontName = "xft:monospace-20"
    -- , decoHeight = 30
    --, activeColor         = "#999999"
    --, inactiveColor       = "#666666"
    --, activeBorderColor   = "#999999"
    --, inactiveBorderColor = "#666666"

    -- adapted from XMonad.Util.Themes.smallClean
      , fontName            = "xft:Cantarell:bold:size=22"
      , activeColor         = "#8a999e"
      , inactiveColor       = "#545d75"
      , activeBorderColor   = "#8a999e"
      , inactiveBorderColor = "#545d75"
      --, activeBorderColor   = "white"
      --, inactiveBorderColor = "grey"
      , activeTextColor     = "white"
      , inactiveTextColor   = "grey"
      , decoHeight          = 30
    }

imageButtonDeco :: (Eq a, Shrinker s) => s -> Theme
                   -> l a -> ModifiedLayout (Decoration ImageButtonDecoration s) l a
imageButtonDeco s c = decoration s c $ NFD True

newtype ImageButtonDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageButtonDecoration a where
    describeDeco _ = "ImageButtonDeco"
    decorationCatchClicksHook _ = imageTitleBarButtonHandler
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()

imageButtonTabbed
    :: (Eq a, Shrinker s)
    => s -> Theme
    -> ModifiedLayout (Decoration TabbedImageButtonDecoration s) Simplest a
imageButtonTabbed s c = decoration s c TabbedImageButton Simplest

data TabbedImageButtonDecoration a = TabbedImageButton
    deriving (Show, Read)

instance Eq a => DecorationStyle TabbedImageButtonDecoration a where
    describeDeco TabbedImageButton = "TabbedImageButton"

    decorationEventHook ds dst ButtonEvent { ev_window     = ew
                                         , ev_event_type = et
                                         , ev_button     = eb
                                         , ev_x_root     = ex
                                         , ev_y_root     = ey }
        | et == buttonPress
        , Just ((mainw,r), (_, decoRectM)) <- findWindowByDecoration ew dst = do
            focus mainw
            -- below from 'XMonad.Layout.Decoration.handleMouseFocusDrag'
            -- (internal function!)
            let Rectangle dx _ dwh _ = fromJust decoRectM
                distFromLeft = ex - fi dx
                distFromRight = fi dwh - (ex - fi dx)
            dealtWith <- decorationCatchClicksHook ds mainw (fi distFromLeft) (fi distFromRight)
            unless dealtWith $
                mouseDrag (\x y -> focus mainw >> decorationWhileDraggingHook ds ex ey (mainw, r) x y)
                            (decorationAfterDraggingHook ds (mainw, r) ew)
    decorationEventHook _ _ _ = return ()

    decorationCatchClicksHook _ = imageTitleBarButtonHandler

    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()

    pureDecoration TabbedImageButton wt ht _ s wrs (w,r@(Rectangle x y wh hh))
        -- = if numWindows > 1
        --   then Just upperTab
        --   else Nothing
        = Just upperTab
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (S.integrate s)
              loc k h i = k + fi ((h * fi i) `div` max 1 (fi $ length ws))
              esize k h = fi $ maybe k (\i -> loc k h (i+1) - loc k h i) $ w `elemIndex` ws
              wid = esize x wh
              n k h = maybe k (loc k h) $ w `elemIndex` ws
              nx = n x wh
              upperTab = Rectangle nx  y wid (fi ht)
              -- lowerTab = Rectangle nx (y + fi (hh - ht)) wid (fi ht)
              -- fixHeightLoc i = y + fi ht * fi i
              -- fixHeightTab k = Rectangle k
              --   (maybe y fixHeightLoc
              --    $ w `elemIndex` ws) (fi wt) (fi ht)
              -- rightTab = fixHeightTab (x + fi (wh - wt))
              -- leftTab = fixHeightTab x
              numWindows = length ws
    shrink TabbedImageButton (Rectangle _ _ dw dh) (Rectangle x y w h)
        = Rectangle x (y + fi dh) w (h - dh)
            -- D -> Rectangle x y w (h - dh)
            -- L -> Rectangle (x + fi dw) y (w - dw) h
            -- R -> Rectangle x y (w - dw) h
