{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}

module Modules.Keys where
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.BoringWindows as B
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import XMonad.Util.ExclusiveScratchpads
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import qualified Data.Map as M
import XMonad.Layout.IndependentScreens
import XMonad.Util.Loggers
import Control.Monad
import XMonad.Actions.PhysicalScreens
import Data.List
import Data.Char
import XMonad.Actions.KeyRemap
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.Paste

import Modules.Others (dtXPConfig', exclusiveSps, dtXPConfig, tsDefaultConfig, promptList)
import Modules.MyTreeSelect (treeselectAction, sshTreeselectAction)

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------

spawnRectScrot = spawn "sleep 0.2 && scrot -s ~/media/screenshots/%y-%m-%d_%H-%M-%S.png && notify-send chink-rec"
spawnScrot     = spawn "scrot ~/media/screenshots/%y-%m-%d_%H-%M-%S.png && notify-send chink"
spawnRecompile = spawn "xmonad --recompile && xmonad --restart"
spawnRestart   = spawn "shutdown -r now"
spawnShutdown  = spawn "shutdown now"

emacsKeys :: [(String, X())]
emacsKeys = [
  ------------------------------- XFKeys ----------------------------------------
      ("<XF86AudioStop>", spawn "xterm")
    , ("<XF86AudioPrev>", spawn "xterm")
    , ("<XF86AudioNext>", spawn "xterm")
    , ("<XF86AudioPlay>", spawn "cplay")
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -15")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +15")


  ------------------------------- Programs ----------------------------------------
    , ("M4-S-<Return>", shellPrompt dtXPConfig)
    , ("C-M1-a"  , spawn "atom"           )
    , ("C-M1-c"  , spawn "calibre"        )
    , ("C-M1-d"  , spawn "dropbox"        )
    , ("C-M1-e"  , spawn "evince"         )
    , ("C-M1-f"  , spawn "firefox"        )
    , ("C-M1-S-f", spawn "firefox-developer-edition" )
    , ("C-M1-i"  , spawn "idea"           )
    , ("C-M1-j"  , spawn "joplin-desktop" )
    , ("C-M1-l"  , spawn "libreoffice"    )
    , ("C-M1-p"  , spawn "pycharm"        )
    , ("C-M1-p"  , spawn "rambox"         )
    , ("C-M1-s"  , spawn "slack"          )
    , ("C-M1-t"  , spawn "xterm"          )
    , ("C-M1-v"  , spawn "virtualbox"     )
    , ("C-M1-z"  , spawn "zotero"         )

  ------------------------------- Scratchpads ----------------------------------------
    , ("C-M-c", scratchpadAction exclusiveSps "cmus"      )
    , ("C-M-d", scratchpadAction exclusiveSps "cal"       )
    , ("C-M-g", scratchpadAction exclusiveSps "ghci"      )
    , ("C-M-h", scratchpadAction exclusiveSps "htop"      )
    , ("C-M-m", scratchpadAction exclusiveSps "rambox"    )
    , ("C-M-n", scratchpadAction exclusiveSps "mailspring")
    , ("C-M-p", scratchpadAction exclusiveSps "python"    )
    , ("C-M-s", scratchpadAction exclusiveSps "scala"     )
    , ("C-M-t", scratchpadAction exclusiveSps "trello"    )
    , ("C-M-v", scratchpadAction exclusiveSps "pulse"     )
    , ("C-M-w", scratchpadAction exclusiveSps "todoist"   )

  ------------------------------- Layouts ----------------------------------------
    , ("M-u"  , moveToIndependent Prev      )
    , ("M-i"  , moveToIndependent Next      )
    , ("M-k"  , B.focusDown                 )
    , ("M-j"  , B.focusUp                   )
    , ("M-h"  , sendMessage Shrink          )
    , ("M-l"  , sendMessage Expand          )
    , ("M-<Tab>"  , sendMessage NextLayout  )
    , ("M-S-<Tab>", sendMessage FirstLayout )

    , ("M-z"  , toggleScreen                    )
    , ("M-S-z"  , shiftNextScreen >> toggleScreen)
    , ("M-S-k", windows W.swapDown              )
    , ("M-S-j", windows W.swapUp                )
    , ("M-S-c", kill1                           )
    , ("M-S-a", killAll                         )
    , ("M-t t", treeselectAction tsDefaultConfig)
    , ("M-s s", sshTreeselectAction tsDefaultConfig)

  ------------------------------- Others ----------------------------------------
    , ("M-M1-h", sendMessage $ pullGroup L) -- TODO SubLayouts!
    , ("M-M1-l", sendMessage $ pullGroup R)
    , ("M-M1-k", sendMessage $ pullGroup U)
    , ("M-M1-j", sendMessage $ pullGroup D)

    , ("M-M1-m", withFocused $sendMessage . MergeAll)
    , ("M-M1-u", withFocused $sendMessage . UnMerge)

    , ("M-M1-,", onGroup W.focusUp')
    , ("M-M1-;", onGroup W.focusDown')
--    , ("C-m", sendKey shiftMask xK_Return) -- TODO remap return CR enter

  ------------------------------- Functions ----------------------------------------
    , ("M1-<F4>"  ,     spawnShutdown )
    , ("M1-<F5>"  ,     spawnRestart  )
    , ("<Print>"  ,     spawnScrot    )
    , ("S-<Print>",     spawnRectScrot)
    , ("M-S-r"    ,     spawnRecompile)
    ]

  ------------------------------- Prompts ----------------------------------------
    ++ [ ("M-p " ++ k, f dtXPConfig') | (k, f) <- promptList ]
    
  ------------------------------- DEBUG ----------------------------------------
    ++ [
--      ("M-r", spawn $ "notify-send " ++ show (marshall 0 $ head clickables))
	  ("M-r", spawn "notify-end DEBUG")
--  , ("M-r", spawn $ "notify-send var")
    ]
 
-- Spaghetti
physicalScreens :: X [Maybe ScreenId]
physicalScreens = withWindowSet $ \windowSet -> do
	let numScreens = length $ W.screens windowSet
	mapM (\s -> getScreen def (P s)) [0..numScreens]

getPhysicalScreen :: ScreenId -> X (Maybe PhysicalScreen)
getPhysicalScreen sid = do
	pscreens <- physicalScreens
	return $ (Just sid) `elemIndex` pscreens >>= \s -> Just (P s)

screenID :: X Char
screenID = withWindowSet $ \windowSet -> do
	let sid = W.screen (W.current windowSet)
	pscreen <- getPhysicalScreen sid
	return $ case pscreen of
				Just (P s) -> (head (show s))
				otherwise  -> 'r'

-- XORG counts from left to right, X.L.IndependentScreens from right to left
flipBit :: Char -> Char
flipBit c = intToDigit $ 1 - digitToInt c

moveToIndependent :: Direction1D -> X ()
moveToIndependent dir = do
	id <- screenID
	moveTo dir $ WSTagGroup $ flipBit id

toggleScreen :: X ()
toggleScreen = do
	id <- screenID
	case id of
	  '0' -> nextScreen >> shiftMouse "right"
	  '1' -> prevScreen >> shiftMouse "left"
	


shiftMouse :: String -> X()
shiftMouse direction
    | direction == "left"  = spawn "exec xdotool mousemove_relative -- -1920 0"
    | direction == "right" = spawn "exec xdotool mousemove_relative    +1920 0"

mouseKeys :: [((ButtonMask, Button), Window -> X ())]
mouseKeys = [
      ((0, 6), \w -> moveToIndependent Prev )
    , ((0, 7), \w -> moveToIndependent Next )
    ]


------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
confKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
confKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $ [
    ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) azertyKeys
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]

    ] ++ [ -- Don't remove - For xdotooling clickables
    ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) fKeys
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

azertyKeys, fKeys :: [KeySym]
azertyKeys = [0x26, 0xe9, 0x22, 0x27, 0x28, 0x2d, 0xe8, 0x5f, 0xe7]
fKeys      = [xK_F1 .. xK_F9]

strWorkspaces, strFKeys, clickables :: [String]
strFKeys      = ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"]
strWorkspaces = ["1:chess", "2:dev", "3:job", "4:**", "5:***", "6:**", "7:lang", "8:alf", "9:dump"]
--strWorkspaces = ["ide", "dev", "job", "edu", "vid", "org", "lang", "pm", "dump"]
--strWorkspaces = ["ide", "dev", "cours", "misc", "*", "misc", "lang", "xmon", "sys"]

clickables = action strWorkspaces
    where action l = [ "<action=xdotool key super+" ++ n ++ ">" ++ ws ++ "</action>" |
                     (i, ws) <- zip strFKeys l, let n = i]
