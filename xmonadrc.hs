{-# LANGUAGE NamedFieldPuns #-}

import XMonad

import XMonad.Actions.Volume
import XMonad.Config.Xfce
import XMonad.Hooks.ShowWName (flashName)
import XMonad.Layout.DragPane
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W

import MyImageButtonDecoration

import System.Exit

import Data.Map.Strict (fromList)

main :: IO ()
main = xmonad $ base
    { modMask = mod4Mask
    --, focusedBorderColor = "#444444"
    , normalBorderColor  = "#666666"
    , focusedBorderColor = "#999999"
    , borderWidth = 10
    , layoutHook = layout
    , keys = overrideKeys <> keys base
--    , manageHook = composeAll
--        [ manageHook base
--        --, className =? "Wrapper-2.0" --> doIgnore
--        --, appName =? "full-history-search" --> doFloat
--        ]
    }
  where
    base = xfceConfig

    -- adapted from base
    layout = xfcePanelGap $
        imageButtonTabbed shrinkText defaultThemeWithImageButtons
        ||| addDeco
            (   toggleLayouts (tiledrag Vertical)   tiled
            ||| toggleLayouts (tiledrag Horizontal) (Mirror tiled)
            )
      where
        -- hack, see https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Does_xmonad_support_a_statusbar.3F
        xfcePanelGap = gaps [(D,54)]

        tabtheme :: Theme
        tabtheme = def
            { fontName = "xft:monospace-20"
            , decoHeight = 20
            , activeColor         = "#999999"
            , inactiveColor       = "#666666"
            , activeBorderColor   = "#999999"
            , inactiveBorderColor = "#666666"
            }

        -- add window titles
        addDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons 

        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        tiledrag dir = dragPane dir delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio :: Fractional a => a
        ratio   = 1/2

        -- Percent of screen to increment by when resizing panes
        delta :: Fractional a => a
        delta   = 3/100

    overrideKeys = \c@(XConfig{modMask}) -> fromList $
        [ ((modMask .|. shiftMask, xK_p), spawn "dmenu_run -fn 'monospace-20'")
        , ((modMask, xK_p), spawn "xfce4-appfinder")
        , ((modMask .|. shiftMask, xK_q), spawn "xfce4-session --logout" >> liftIO exitSuccess)
        , ((modMask, xK_z), flashName def)
        , ((modMask, xK_x), spawn "/usr/bin/xfce4-panel --restart")
        , ((modMask .|. controlMask, xK_space), sendMessage ToggleLayout)
        ]
        -- ++
        -- -- replace greedyView with View in mod-(shift-)n
        -- [((m .|. modMask, k), windows $ f i)
        -- | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
        ++
        -- adapted from XMonad source:
        -- for my setup need to switch w/e
        -- also a/s/d easier for me than w/e/r
        [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_s, xK_a, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

        -- [ ((modMask, xK_q), spawn
        --     "/home/bradrn/.config/xmonad/xmonad-x86_64-linux --recompile && /home/bradrn/.config/xmonad/xmonad-x86_64-linux --restart")
