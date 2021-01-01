{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}

module Modules.Layouts where
import XMonad

import qualified XMonad.Layout.BoringWindows as B
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts)

    -- Layouts modifiers
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger (windowArrange)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
    -- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile

import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W
import Data.Monoid
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Minimize


import Modules.Others (clickables)


------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
--mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
--mySpacing i = smartSpacing 0 --False (Border 0 0 5 5) True (Border 0 0 5 5) True
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           -- $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           -- $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
spirals  = renamed [Replace "spirals"]
           -- $ mySpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           -- $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
horizontal = renamed [Replace "horizontal"]
           $ limitWindows 4
           $ Mirror
           $ ResizableTall 1 (3/100) (1/2) []

-- The layout hook
myLayoutHook = B.boringWindows      -- exclusiveSps
             $ minimize             -- exclusiveSps
             $ avoidStruts
            -- $ mouseResize
             $ windowArrange
             $ T.toggleLayouts floats
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ myDefaultLayout
             where myDefaultLayout = tall
                                  ||| noBorders monocle
                                  ||| grid
                                  ||| horizontal
                                  --- ||| threeCol
                                  --- ||| threeRow
                                  -- ||| noBorders tabs
                                  --- ||| spirals


------------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = (isDialog --> doF W.swapUp) <+> composeAll
    [
       title =? "xmessage"                     --> doCenterFloat
    ,  title =? "Oracle VM VirtualBox Manager" --> doFloat
    ,  title =? "pulse"                        --> doFloat
    ,  title =? "Spectacle Editor Setup "      --> doFloat
    ,  title =? "Zotero Preferences"                        --> doFloat
    ,  title =? "self_driving_car_nanodegree_program"     --> doFloat
    , stringProperty "_NET_WM_WINDOW_TYPE" =? "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat
    ,  resource   =? "synergy"                 --> doFloat
    ,  (className =? "firefox"
     <&&> resource =? "Dialog")                --> doFloat
    ,  title =? "jetbrains-idea-ce"            --> doShift (marshall 0 (clickables !! 0))
    ,  className =? "jetbrains-pycharm-ce"     --> doShift (marshall 0 (clickables !! 0))
    , isFullscreen --> doFullFloat
    ]


