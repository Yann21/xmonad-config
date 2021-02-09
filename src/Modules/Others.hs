{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}

module Modules.Others where
import XMonad
import qualified XMonad.Actions.TreeSelect as TS
import qualified XMonad.StackSet as W
import XMonad.Util.ExclusiveScratchpads

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Data.List
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import System.IO (hPutStrLn)
import qualified Data.Map as M
import XMonad.Layout.IndependentScreens


import Modules.MyTreeSelect (myTreeNavigation)

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
m, altMask :: KeyMask
altMask = mod1Mask
m       = mod4Mask

myFont        = "xft:UbuntuMono Nerd Font:bold:pixelsize=15"
myTerminal    = "xterm"
myNormColor   = "#221710" -- original: 292d3e
myFocusColor  = "#bbc5ff"
myBorderWidth = 3
myBrowser     = "firefox"

pp handle s = marshallPP s xmobarPP {
      ppOutput          = hPutStrLn handle
    , ppCurrent         = xmobarColor "#AEDEA6" "" . wrap "[" "]"
    , ppVisible         = xmobarColor "#fdbed2" "" . wrap "[" "]"
    , ppHidden          = xmobarColor "#A9C3FC" ""
    , ppHiddenNoWindows = xmobarColor "#9F9F9F" ""
    , ppTitle           = xmobarColor "#FFF8F0" "" . shorten 60
    , ppSep             =         "<fc=#666666> | </fc>"
    , ppUrgent          = xmobarColor "#C45500" ""
--    , ppExtras          = getXmonadDir
    , ppOrder           = \(ws:l:t:ex) -> [ws, l]
}


------------------------------------------------------------------------
-- XPROMPT SETTINGS (dmenu)
------------------------------------------------------------------------
dtXPConfig, dtXPConfig' :: XPConfig
dtXPConfig = def {
      font                = "xft:UbuntuMono Nerd Font:size=12"
    , bgColor             = "#292d3e"
    , fgColor             = "#d0d0d0"
    , bgHLight            = "#c792ea"
    , fgHLight            = "#000000"
    , borderColor         = "#535974"
    , promptBorderWidth   = 0
    , position            = Bottom
    , height              = 20
    , historySize         = 256
    , historyFilter       = id
    , defaultText         = []
    , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
    , showCompletionOnTab = False
    , searchPredicate     = isPrefixOf
    , alwaysHighlight     = True
    , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
}

dtXPConfig' = dtXPConfig {
    autoComplete = Nothing
}

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig
    { TS.ts_hidechildren = True
    , TS.ts_background   = 0xdd292d3e
    , TS.ts_font         = myFont
    , TS.ts_node         = (0xffd0d0d0, 0xff202331)
    , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
    , TS.ts_highlight    = (0xffffffff, 0xff755999)
    , TS.ts_extra        = 0xffd0d0d0
    , TS.ts_node_width   = 200
    , TS.ts_node_height  = 20
    , TS.ts_originX      = 0
    , TS.ts_originY      = 20
    , TS.ts_indent       = 80
    , TS.ts_navigate     = myTreeNavigation
    }


-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [
	  ("m", manPrompt)          -- manpages prompt
    , ("p", passPrompt)         -- get passwords (requires 'pass')
    , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
    , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
    , ("s", sshPrompt)          -- ssh prompt
    , ("x", xmonadPrompt)       -- xmonad prompt
    ]


------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
exclusiveSps :: ExclusiveScratchpads
exclusiveSps = mkXScratchpads [
      ("cal" , "google-calendar",                   resource =? "google-calendar-nativefier-e22938")
    , ("cmus", "xterm -name cmus cmus",             resource =? "cmus")
    , ("ghci", "xterm -name ghci -e 'stack ghci'",  resource =? "ghci")
    , ("htop", "xterm -bg black -name htop htop",   resource =? "htop")
    , ("jshell" , "xterm -name jshell jshell",                           resource =? "jshell")
    , ("geary" , "geary",                           resource =? "geary")
    , ("pulse" , "xterm -name pulsemixer pulsemixer", resource =? "pulsemixer")
    , ("python", "xterm -fs 17 -name python bpython", resource =? "python")
    , ("rambox", "rambox",                          resource =? "rambox")
    , ("scala" , "xterm -name scala scala",         resource =? "scala" )
    , ("trello", "trello",                          resource =? "trello")
    , ("todoist", "todoist",                        resource =? "todoist")
    , ("iotop", "xterm -bg orange -name iotop iotop", resource =? "iotop")
    ] $ customFloating $ W.RationalRect 0.15 0.15 0.7 0.7
