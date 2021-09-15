{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}

module Modules.Keys where
import Control.Monad
import Data.Char
import Data.List
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.KeyRemap
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WithAll (killAll)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Util.ExclusiveScratchpads
import XMonad.Util.Loggers
import XMonad.Util.Paste
import qualified Data.Map as M
import qualified XMonad.Layout.BoringWindows as B
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.StackSet as W

import Modules.Others (dtXPConfig', dtXPConfig, tsDefaultConfig, promptList)
import Modules.MyTreeSelect (treeselectAction, sshTreeselectAction)
------------------------------------------------------------------------
-- COMMANDS
------------------------------------------------------------------------
timeFormat = "%y-%m-%d_%H-%M-%S"
spawnRectScrotClipboard = spawn $ "sleep 0.2 && scrot -s ~/Documents/Media/screenshots/" ++ timeFormat ++ ".png \
    \-e 'xclip -select clipboard -target image/png -i $f'"
spawnRectScrot = spawn $ "sleep 0.2 && scrot -s ~/Documents/Media/screenshots/" ++ timeFormat ++ ".png && notify-send chink-rec"
spawnScrot     = spawn $ "scrot ~/Documents/Media/screenshots/" ++ timeFormat ++ ".png && notify-send chink"
spawnRecompile = spawn "xmonad --recompile && xmonad --restart"
spawnXkill = spawn "xkill"
spawnXprop = spawn "xterm -name float -e '/home/yann/system/bin/xprop_4_xmonad.sh && sh'"
spawnHibernate  = spawn "xterm -e systemctl hibernate"
spawnShutdown  = spawn "shutdown now"
shiftWindowCommand = "exec xdotool getactivewindow windowmove --relative"
shiftWindow dir
    | dir == "up"    = spawn $ shiftWindowCommand ++ "+0 -75"
    | dir == "down"  = spawn $ shiftWindowCommand ++ "+0  +75"
    | dir == "left"  = spawn $ shiftWindowCommand ++ "-75 +0"
    | dir == "right" = spawn $ shiftWindowCommand ++ "+75 +0"

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
emacsKeys :: [(String, X())]
emacsKeys = [
  ------------------------------- Programs ----------------------------------------
      --("M-S-m"   , spawn "ulauncher")
      ("C-M1-S-f", spawn "firefox-developer-edition" )
    , ("C-M1-a"  , spawn "atom"           )
    , ("C-M1-c"  , spawn "calibre"        )
    , ("C-M1-e"  , spawn "emacs"          )
    , ("C-M1-f"  , spawn "firefox"        )
    , ("C-M1-g"  , spawn "geogebra"       )
    , ("C-M1-i"  , spawn "idea-ce"        )
    , ("C-M1-k"  , spawn "anki"           )
    , ("C-M1-n"   , spawn "nautilus"       )
    --, ("C-M1-l"  , spawn "libreoffice"    ) -- conflict with intellij align code
    , ("C-M1-p"  , spawn "$HOME/Code/tools/Pycharm2019/pycharm-2019.3.4/bin/pycharm.sh"        )
    , ("C-M1-r"  , spawn "rstudio-bin"        )
    , ("C-M1-t"  , spawn "xterm"          )
    , ("C-M1-v"  , spawn "virtualbox"     )

    -- TODO Reorganize bindings
  ------------------------------- Scratchpads ----------------------------------------
    , ("C-M-<Space>", scratchpadAction exclusiveSps "xterm" )
    , ("C-M-b", scratchpadAction exclusiveSps "todoist"	    ) -- b
    , ("C-M-c", scratchpadAction exclusiveSps "cal"         ) -- [c]alendar
    , ("C-M-d", scratchpadAction exclusiveSps "stardict"    ) -- [d]ictionary
    , ("C-M-e", scratchpadAction exclusiveSps "virt-manager") -- [e]mulator
    , ("C-M-g", scratchpadAction exclusiveSps "nvtop"	    ) -- [g]pu
    , ("C-M-h", scratchpadAction exclusiveSps "htop"	    ) -- [h]top
    , ("C-M-i", scratchpadAction exclusiveSps "hardinfo"    ) -- hard[i]nfo
    , ("C-M-j", scratchpadAction exclusiveSps "jshell"	    ) -- java
    , ("C-M-k", scratchpadAction exclusiveSps "anki"	    ) -- an[k]i
    , ("C-M-m", scratchpadAction exclusiveSps "thunderbird" ) -- [m]ail
    , ("C-M-n", scratchpadAction exclusiveSps "ao"	    ) -- [n]otes
    , ("C-M-o", scratchpadAction exclusiveSps "octave"	    ) -- octave
    , ("C-M-p", scratchpadAction exclusiveSps "python"	    ) -- python
    , ("C-M-r", scratchpadAction exclusiveSps "R"	    ) -- R
    , ("C-M-s", scratchpadAction exclusiveSps "rambox"      ) -- [s]ocial media
    , ("C-M-t", scratchpadAction exclusiveSps "trello"      ) -- trello
    , ("C-M-v", scratchpadAction exclusiveSps "pulse"       ) -- volume
    , ("C-M-x", scratchpadAction exclusiveSps "spotify"     ) -- x
    , ("C-M-w", scratchpadAction exclusiveSps "cmus"        ) -- w
    , ("C-M-z", scratchpadAction exclusiveSps "zotero"      ) -- zotero
    -- , ("C-M-s", scratchpadAction exclusiveSps "scala"   )

  ------------------------------- Layouts ----------------------------------------
    , ("M-k"  , B.focusDown                      )
    , ("M-j"  , B.focusUp                        )
    , ("M-h"  , sendMessage Shrink               )
    , ("M-l"  , sendMessage Expand               )
    , ("M-<Tab>"  , sendMessage NextLayout       )
    , ("M-S-<Tab>", sendMessage FirstLayout      )

    , ("M-a"  , moveToIndependent Prev           )
    , ("M-e"  , moveToIndependent Next           )
    , ("M-z"  , toggleScreen                     )
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

    , ("M-M1-m", withFocused $ sendMessage . MergeAll)
    , ("M-M1-u", withFocused $ sendMessage . UnMerge )

    , ("M-M1-,", onGroup W.focusUp'  )
    , ("M-M1-;", onGroup W.focusDown')

  ------------------------------- Functions ----------------------------------------
    , ("<F4>"     ,     spawnXkill    )
    , ("M1-<F3>"  ,     spawnHibernate)
    , ("M1-<F4>"  ,     spawnShutdown )
    , ("<Print>"  ,     spawnScrot    )
    , ("S-<Print>",     spawnRectScrot)
    , ("C-<Print>",     spawnRectScrotClipboard)
    , ("M-S-r"    ,     spawnRecompile)
    , ("<F5>"     ,     spawnXprop)

  ------------------------------- XFKeys ----------------------------------------
    , ("<XF86AudioStop>", spawn "xterm")
    , ("<XF86AudioPrev>", spawn "xterm")
    , ("<XF86AudioNext>", spawn "xterm")
    , ("<XF86AudioPlay>", spawn "cplay")
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -15")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +15")

  ------------------------------- Yeelight ----------------------------------------
  -- TODO: scripts use directory structure
    , ("S-<XF86AudioMute>",        spawn "~/system/bin/yeelight/controller.py toggle")
    , ("S-<XF86AudioLowerVolume>", spawn "~/system/bin/yeelight/controller.py down")
    , ("S-<XF86AudioRaiseVolume>", spawn "~/system/bin/yeelight/controller.py up")
    , ("M1-<U>", spawn "xdotool getactivewindow windowmove --relative 0 -100")
    , ("M1-<D>", spawn "xdotool getactivewindow windowmove --relative 0 +100")
    , ("M1-<L>", spawn "xdotool getactivewindow windowmove --relative -- -100 0")
    , ("M1-<R>", spawn "xdotool getactivewindow windowmove --relative +100 0")
    --, ("S-<XF86AudioStop>", spawn "")
    --, ("S-<XF86AudioPrev>", spawn "")
    --, ("S-<XF86AudioNext>", spawn "")
    --, ("S-<XF86AudioPlay>", spawn "")
    , ("M-<Space>", goToSelected defaultGSConfig)
    ]

  ------------------------------- Prompts ----------------------------------------
    ++ [ ("M-p " ++ k, f dtXPConfig') | (k, f) <- promptList ]
    
  ------------------------------- DEBUG ----------------------------------------
    ++ [
--      ("M-r", spawn $ "notify-send " ++ show (marshall 0 $ head clickables))
	  ("M-r", spawn "notify-end DEBUG")
--  , ("M-r", spawn $ "notify-send var")
    ]

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
exclusiveSps :: ExclusiveScratchpads
exclusiveSps = mkXScratchpads [
      ("ao",            "ao",                                   resource =? "ao")
    , ("anki" ,         "anki",                                 resource =? "anki")
    , ("cal" ,          "google-calendar-nativefier",           resource =? "googlecalendar-nativefier-e22938")
    , ("cmus",          "xterm -name cmus cmus",                resource =? "cmus")
    , ("thunderbird" ,  "thunderbird",                          resource =? "Mail")
    , ("nvtop",         "xterm -name nvtop nvtop",		resource =? "nvtop")
    , ("htop",          "xterm -bg black -name htop htop",      resource =? "htop")
    , ("hardinfo",      "hardinfo",				resource =? "hardinfo")
    , ("jshell",        "xterm -name jshell jshell",            resource =? "jshell")
    , ("octave" ,       "xterm -fs 16 -name octave octave",     resource =? "octave")
    , ("pulse" ,        "xterm -name pulsemixer pulsemixer",    resource =? "pulsemixer")
    , ("python",        "xterm -fs 16 -name python ptpython",   resource =? "python")
    , ("R",             "xterm -fs 16 -name R R",               resource =? "R")
    , ("rambox",        "rambox",                               resource =? "rambox")
    , ("scala" ,        "xterm -name scala scala",              resource =? "scala" )
    , ("spotify",       "spotify",                              resource =? "spotify")
    , ("stardict",      "startdict",                            resource =? "stardict")
    , ("todoist",       "todoist",                              resource =? "todoist")
    , ("trello",        "trello",                               resource =? "trello")
    , ("xterm" ,        "xterm -name scratch",			appName	 =? "scratch")
    , ("virt-manager" , "virt-manager",                         title    =? "Virtual Machine Manager") -- differentiate between the launcher and the VMs
    , ("zotero",        "zotero",                               title    =? "Zotero")
    ] $ customFloating $ W.RationalRect 0.15 0.15 0.7 0.7




shiftMouse :: String -> X()
shiftMouse direction
    | direction == "left"  = spawn "exec xdotool mousemove_relative -- -1920 0"
    | direction == "right" = spawn "exec xdotool mousemove_relative    +1920 0"

mouseKeys :: [((ButtonMask, Button), Window -> X ())]
mouseKeys = [
      ((0, 6), \w -> moveToIndependent Prev )
    , ((0, 7), \w -> moveToIndependent Next )
    , ((mod4Mask, button2), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
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
strWorkspaces = ["1:idea", "2:*", "3:*", "4:**", "5:*",
                 "6:project", "7:stox", "8:candy", "9:anki"]
--strWorkspaces = ["ide", "dev", "job", "edu", "vid", "org", "lang", "pm", "dump"]
--strWorkspaces = ["ide", "dev", "cours", "misc", "*", "misc", "lang", "xmon", "sys"]

clickables = action strWorkspaces
    where action l = [ "<action=xdotool key super+" ++ n ++ ">" ++ ws ++ "</action>" |
                     (i, ws) <- zip strFKeys l, let n = i]


-- Spaghetti to separate monitors
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
-- End spaghetti
