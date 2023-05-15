{-# LANGUAGE NamedFieldPuns #-}

import XMonad

import XMonad.Actions.Volume
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.RotSlaves
import XMonad.Config.Xfce
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Hooks.ShowWName (flashName)
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import qualified XMonad.StackSet as W

import MyImageButtonDecoration

import Data.Ratio
import System.Exit

import Data.Map.Strict (fromList)

main :: IO ()
main = xmonad $ base
    { modMask = mod4Mask
    --, focusedBorderColor = "#444444"
    --, normalBorderColor  = "#666666"
    --, focusedBorderColor = "#999999"
    --, focusedBorderColor         = "#dfdcd8"
    --, normalBorderColor       = "#f6f5f4"
    , focusedBorderColor   = activeColor defaultThemeWithImageButtons
    , normalBorderColor    = inactiveColor defaultThemeWithImageButtons
    , borderWidth = 10
    , layoutHook = layout
    , keys = overrideKeys <> keys base
    , manageHook = composeAll
        [ manageHook base
        , className =? "Xfce4-appfinder" --> doRectFloat (W.RationalRect (1%4) (1%4) (1%2) (1%2))
        , className =? "Pavucontrol" --> doFloat
        , className =? "Nvpy" --> doFloat
        , className =? "Xfce4-notifyd" --> doIgnore
        --, className =? "Wrapper-2.0" --> doIgnore
        --, appName =? "full-history-search" --> doFloat
        ]
    }
  where
    base = xfceConfig

    -- adapted from base
    layout = desktopLayoutModifiers $
        toggleLayouts
            (noBorders Full)
            (imageButtonTabbed shrinkText defaultThemeWithImageButtons)
        ||| addDeco
            (   toggleLayouts tiledrag tiled
            ||| toggleLayouts (Mirror tiledrag) (Mirror tiled)
            )
      where
        -- add window titles
        addDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons 

        -- default tiling algorithm partitions the screen into two panes
        tiled    = ResizableTall nmaster delta ratio []
        tiledrag = TwoPane delta ratio

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
        , ((modMask, xK_Tab), rotSlavesUp)
        , ((modMask, xK_Pause), kill)
        , ((modMask .|. shiftMask, xK_h), sendMessage MirrorExpand)
        , ((modMask .|. shiftMask, xK_l), sendMessage MirrorShrink)
        , ((modMask, xK_v), spawn "gvim")
        , ((modMask .|. shiftMask, xK_b), spawn "dolphin")
        ]
        -- ++
        -- -- replace greedyView with View in mod-(shift-)n
        -- [((m .|. modMask, k), windows $ f i)
        -- | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
        ++
        -- adapted from XMonad.Actions.PhysicalScreens doumentation:
        -- for my setup need to order by physical screen arrangement
        -- also a/s/d easier for me than w/e/r
        [((m .|. modMask, key), f sc)
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]

        -- [ ((modMask, xK_q), spawn
        --     "/home/bradrn/.config/xmonad/xmonad-x86_64-linux --recompile && /home/bradrn/.config/xmonad/xmonad-x86_64-linux --restart")
