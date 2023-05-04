{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}

------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import XMonad.Hooks.InsertPosition

--import XMonad.Util.ExclusiveScratchpads
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers

    -- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import Data.Maybe (maybeToList)
import Control.Monad
import XMonad.Actions.KeyRemap

    -- my Imports
--import Modules.MyTreeSelect (treeselectAction, sshTreeselectAction, myTreeNavigation)
import Modules.Keys (emacsKeys, mouseKeys, clickables, confKeys, exclusiveSps, strWorkspaces)
import Modules.Others (dtXPConfig, dtXPConfig', tsDefaultConfig, pp, m)
import Modules.Layouts (myManageHook, myLayoutHook)
import XMonad.Hooks.RefocusLast
import qualified Data.Map.Strict as M



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
    handles <- mapM (\(x, y) -> spawnPipe (xmobarCommand x y)) [(s, nScreens) | s <- [0..nScreens-1]]

    xmonad $ ewmh def {
          manageHook = myManageHook  -- how windows are opened
              <+> manageDocks --not that important
        , handleEventHook = refocusLastWhen myPred <+> handleEventHook def
              <+> fullscreenEventHook -- for evince/chromium fullscreen
              <+> docksEventHook -- status bar

--             <+> serverModeEventHookCmd
--             <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
--             <+> serverModeEventHook

        , layoutHook         = myLayoutHook -- refocusLastLayoutHook $ myLayoutHook
        , startupHook        = myStartupHook >> addEWMHFullscreen -- Adds EWMH tags to Firefox
        , logHook = refocusLastLogHook <+> mapM_ dynamicLogWithPP (zipWith pp handles [0 .. nScreens])

        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , workspaces         = withScreens nScreens strWorkspaces
        , modMask            = mod4Mask
        , keys               = confKeys -- Workspaces

        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , borderWidth        = myBorderWidth
        , terminal           = "xterm"
    } `additionalKeysP` emacsKeys
      `additionalMouseBindings` mouseKeys
      -- TODO: https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-RefocusLast.html
      -- Apply refocus behavior to floating windows that get shifted to different workspace
      where myPred = refocusingIsActive <||> isFloat

xmobarCommand (S screen) nScreens = unwords ["xmobar", "-x", show screen, myConfig screen]
    where
        myConfig 2 = "$HOME/.config/xmobar/xmobarrc_mid.hs"
        myConfig 0 = "$HOME/.config/xmobar/xmobarrc_left.hs"
        myConfig 1 = "$HOME/.config/xmobar/xmobarrc_right.hs"

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOMOSINT"
    liftIO $ do
       sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "$HOME/system/etc/autostart.sh"
    setWMName "xmonad"
    setWMName "LG3D" --for the JVM
