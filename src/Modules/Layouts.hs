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
import XMonad.Layout.PerWorkspace

import qualified XMonad.StackSet as W
import Data.Monoid
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize

import Control.Applicative((<$>))
import Data.Maybe(fromMaybe)
import Data.List(find)
import XMonad.Actions.SpawnOn
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation


import Modules.Keys (clickables, exclusiveSps)
import XMonad.Util.ExclusiveScratchpads



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
--floats   = renamed [Replace "floats"]
--           $ limitWindows 20 simplestFloat
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
             $ onWorkspace "1_dump" grid
             $ onWorkspace "2_dump" grid
             $ onWorkspace "2_dump" grid
             $ onWorkspace "1_9:dump" grid
             $ onWorkspace "0_9:dump" grid
             $ minimize             -- exclusiveSps
             $ avoidStruts          -- don't cover status bar
             $ smartBorders         -- TODO: doesn't work
             $ subTabbed
             $ windowNavigation
--             $ T.toggleLayouts floats
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               myDefaultLayout
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

-- className / resource / title / appName
-- resource seems to work best
myManageHook =
	composeOne [
		  isDialog -?> insertPosition Above Newer
		, return True -?> insertPosition End Newer
	--  , stringProperty "_NET_WM_WINDOW_TYPE" =? "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat

    ] <+> composeAll [
          title =? "xmessage"           --> doCenterFloat
        , title =? "xscreensaver-demo"  --> doFloat
        , title =? "xscreensaver"       --> doFloat
        , resource =? "net-sourceforge-plantuml-Run" --> doFloat -- TODO
        , title =? "jetbrains-idea-ce"  --> doShift (marshall 0 (head clickables))
        , title =? "ulauncher"          --> doIgnore -- TODO
        , resource =? "pqiv"            --> doFloat
        , resource =? "matplotlib"      --> doFloat
        , title =? "Zotero Preferences" --> doFloat
        , title =? "Zenity"             --> doFloat
		, isFullscreen                  --> doFullFloat
        , isDialog                      --> doCenterFloat
        --, resource =? "sun-awt-X11-XFramePeer"  --> doFloat
        , resource =? "com.talkwalker.main.Main" --> doFloat
        --, resource =? "com.talkwalker.misc.Dashboard" --> doFloat
        , resource =? "xdot" --> doFloat
        , resource =? "float" --> doFloat
        , resource =? "r_x11" --> doFloat

    ] <+> xScratchpadsManageHook exclusiveSps

