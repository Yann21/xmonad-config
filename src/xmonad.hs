{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}

------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import XMonad.Hooks.InsertPosition

import XMonad.Util.ExclusiveScratchpads
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers

    -- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.

    -- my Imports
--import Modules.MyTreeSelect (treeselectAction, sshTreeselectAction, myTreeNavigation)
import Modules.Keys (myKeys, emacsKeys, mouseKeys, clickables, confKeys)
import Modules.Others (dtXPConfig, dtXPConfig', promptList, tsDefaultConfig, exclusiveSps, pp, m)
import Modules.Layouts (myManageHook, myLayoutHook)


myTerminal    = "xterm"
myNormColor   = "#221710" -- original: 292d3e
myFocusColor  = "#bbc5ff"
myBorderWidth = 3


------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    nScreens <- countScreens
    handles  <- mapM (spawnPipe . xmobarCommand) [0..nScreens-1]

    xmonad $ ewmh def {
        manageHook = insertPosition End Newer 
            <+> myManageHook  -- how windows are opened
--            <+> manageDocks
            <+> xScratchpadsManageHook exclusiveSps

        , handleEventHook = 
                 handleEventHook def 
             <+> fullscreenEventHook
             <+> serverModeEventHookCmd 
             <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
             <+> serverModeEventHook 
             <+> docksEventHook
	
        , layoutHook         = myLayoutHook 
        , startupHook        = myStartupHook

        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , workspaces         = withScreens nScreens clickables
        , modMask            = m
        , keys               = confKeys

        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , borderWidth        = myBorderWidth
        , terminal           = myTerminal

        , logHook = mapM_ dynamicLogWithPP $ zipWith pp handles [0 .. nScreens]
    } `additionalKeys` myKeys
      `additionalKeysP` emacsKeys
      `additionalMouseBindings` mouseKeys

xmobarCommand (S screen) = unwords ["xmobar", "-x", show screen, myConfig screen]
    where
        myConfig 0 = "/home/yann/.config/xmobar/xmobarrc_mid.hs"
        myConfig 1 = "/home/yann/.config/xmobar/xmobarrc_left.hs"


------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.xmonad/src/autostart.sh"
    --spawnOnce "emacs &"
    --spawnOnce "pycharm &"
    --spawnOnce "sh ~/.screenlayout/arandrrc_portrait.sh &"
    setWMName "LG3D"

