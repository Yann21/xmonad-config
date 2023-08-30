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

import Data.List

------------------------------------------------------------------------
-- COMMANDS
------------------------------------------------------------------------
timeFormat = "%y-%m-%d_%H-%M-%S"
spawnRectScrotClipboard = spawn $ "sleep 0.2 && scrot -s $HOME/Documents/Media/screenshots/" ++ timeFormat ++ ".png \
    \-e 'xclip -select clipboard -target image/png -i $f'"
spawnRectScrot = spawn $ "sleep 0.2 && scrot -s $HOME/Documents/Media/screenshots/" ++ timeFormat ++ ".png && play /usr/share/sounds/freedesktop/stereo/screen-capture.oga"
spawnScrot     = spawn $ "scrot $HOME/Documents/Media/screenshots/" ++ timeFormat ++ ".png && play /usr/share/sounds/freedesktop/stereo/screen-capture.oga"
spawnRecompile = spawn "xmonad --recompile && xmonad --restart"
spawnXkill = spawn "xkill"
spawnXprop = spawn "xterm -name float -e '$HOME/system/bin/xprop_4_xmonad.sh && sh'"
spawnHibernate  = spawn "xterm -e systemctl hibernate"
spawnShutdown  = spawn "shutdown now"
spawnReboot  = spawn "shutdown -r now"
spawnToggleBluetooth = spawn "bluetoothctl info | grep 'Connected: yes' -q && bluetoothctl power off || bluetoothctl power on"
spawnMeetingsRecorder = spawn $ "simplescreenrecorder --start-hidden --start-recording --settingsfile=/home/yann/.ssr/settings_meetings.conf --no-systray & play /usr/share/sounds/freedesktop/stereo/bell.oga"
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
      ("C-M1-S-f", spawn "firefox-developer-edition" )
    , ("C-M1-a"  , spawn "android-studio")
    , ("C-M1-b"  , spawn "blueberry"   )
    , ("C-M1-c"  , spawn "calibre"     )
    , ("C-M1-d"  , spawn "dbeaver"	       )
    , ("C-M1-e"  , spawn "emacs"       )
    , ("C-M1-f"  , spawn "firefox"     )
    , ("C-M1-g"  , spawn "pinta"	     )
    , ("C-S-M1-g"  , spawn "gimp"	     )
    , ("C-M1-h"  , spawn "homebank"    )
    , ("C-M1-i"  , spawn "idea-ce"     )
    , ("C-M1-k"  , spawn "kdenlive"    )

    -- Isn't Alt+= the align code?
    , ("C-M1-l"  , spawn "libreoffice" ) -- conflict with intellij align code
    , ("C-M1-m"  , spawn "thunderbird" )
    , ("C-M1-n"  , spawn "nautilus"    )
    , ("C-M1-r"  , spawn "rstudio-bin" )
    , ("C-M1-t"  , spawn "xterm"       )
    , ("C-M1-s"  , spawn "simple-scan")
    , ("C-M1-S-t"  , spawn "xterm -e 'ssh work'"       )
    , ("C-M1-v"  , spawn "code --disable-workspace-trust"	       ) -- vscode
    , ("C-M1-w"  , spawn "wireshark"    )

    , ("M-m", spawn "ulauncher-toggle")
    , ("M-S-p", spawn "xterm -e 'disper -c'")

  ------------------------------- Scratchpads ----------------------------------------
    , ("C-M-<Space>", scratchpadAction exclusiveSps "xterm" )
    , ("C-M-b", scratchpadAction exclusiveSps "blueberry"   ) -- b
    , ("C-M-c", scratchpadAction exclusiveSps "cal"	    ) -- [c]alendar
    , ("C-M-d", scratchpadAction exclusiveSps "dbeaver"	    ) -- [d]ocumentation
    , ("C-M-e", scratchpadAction exclusiveSps "virt-manager") -- [e]mulator

    , ("C-M-g", scratchpadAction exclusiveSps "nvtop"	    ) -- [g]pu
    , ("C-M-h", scratchpadAction exclusiveSps "htop"	    ) -- [h]top
    , ("C-M-i", scratchpadAction exclusiveSps "hardinfo"    ) -- hardinfo
    , ("C-M-j", scratchpadAction exclusiveSps "jshell"	    ) -- java
    , ("C-M-a", scratchpadAction exclusiveSps "anki"        ) -- [a]nki
    , ("C-M-k", scratchpadAction exclusiveSps "keycombiner"        ) -- [k]eycombiner
    , ("C-M-l", scratchpadAction exclusiveSps "cutelog"        ) -- cute[l]og

    , ("C-M-m", scratchpadAction exclusiveSps "gtk-launch /usr/share/applications/google-maps-desktop.desktop"	    ) -- music
    , ("C-M-n", scratchpadAction exclusiveSps "ticktick"	    ) -- notes
    , ("C-M-p", scratchpadAction exclusiveSps "python"      ) -- python
    , ("C-M-q", scratchpadAction exclusiveSps "copyq"      ) -- python


    , ("C-M-r", scratchpadAction exclusiveSps "radioswissjazz.AppImage"	    ) -- R
    , ("C-M-s", scratchpadAction exclusiveSps "rambox"	    ) -- social media
    , ("C-M-t", scratchpadAction exclusiveSps "timetrack"   ) -- time tracking

    , ("C-M-v", scratchpadAction exclusiveSps "pulse"	    ) -- volume
    , ("C-M-w", scratchpadAction exclusiveSps "cmus"	    ) -- w
    , ("C-M-x", scratchpadAction exclusiveSps "spotify"	    ) -- x
    , ("C-M-y", scratchpadAction exclusiveSps "youtrack"   )

    , ("C-M-z", scratchpadAction exclusiveSps "zotero"	    ) -- zotero


  ------------------------------- Layouts ----------------------------------------
    , ("M-k"  , B.focusDown                      )
    , ("M-j"  , B.focusUp                        )
    , ("M-h"  , sendMessage Shrink               )
    , ("M-l"  , sendMessage Expand               )
    , ("M-<Tab>"  , sendMessage NextLayout       )
    , ("M-S-<Tab>", sendMessage FirstLayout      )
    , ("M-q"  , moveToIndependent Prev False           )
    , ("M-d"  , moveToIndependent Next False           )
    , ("M-a", switchScreen '0'                   )
    , ("M-z", switchScreen '1'                   )
    , ("M-e", switchScreen '2'                   )
    , ("M-S-a", shiftToScreen '0'                )
    , ("M-S-z", shiftToScreen '1'                )
    , ("M-S-e", shiftToScreen '2'                )
    , ("M-S-k", windows W.swapDown               )
    , ("M-S-j", windows W.swapUp                 )
    , ("M-S-c", kill1                            ) -- / killAll
    , ("M-t t", treeselectAction tsDefaultConfig )
    , ("M-s s", sshTreeselectAction tsDefaultConfig)
    , ("M-<Space>", goToSelected defaultGSConfig )
    , ("M-S-l", spawn "loginctl lock-session"    )

  ------------------------------- Tabefy ----------------------------------------
    , ("M-M1-h", sendMessage $ pullGroup L)
    , ("M-M1-l", sendMessage $ pullGroup R)
    , ("M-M1-k", sendMessage $ pullGroup U)
    , ("M-M1-j", sendMessage $ pullGroup D)
    , ("M-M1-m", withFocused $ sendMessage . MergeAll)
    , ("M-M1-u", withFocused $ sendMessage . UnMerge )
    , ("M-M1-,", onGroup W.focusUp'	)
    , ("M-M1-;", onGroup W.focusDown'	)

  ------------------------------- Functions ----------------------------------------
    , ("<F4>"     ,     spawnXkill	)
    , ("M1-<F3>"  ,     spawnHibernate	)
    , ("M1-<F4>"  ,     spawnShutdown	)
    , ("M1-<F5>"  ,     spawnReboot	)
    , ("<Print>"  ,     spawnScrot	)
    , ("S-<Print>",     spawnRectScrot	)
    , ("C-<Print>",     spawnRectScrotClipboard)
    , ("M-S-r"    ,     spawnRecompile	)
    , ("<F5>"     ,     spawnXprop	)

  ------------------------------- XFKeys ----------------------------------------
    , ("<XF86AudioStop>", spawn "xterm")
    , ("<XF86AudioPrev>", spawn "xterm")
    , ("<XF86AudioNext>", spawn "xterm")
    , ("<XF86AudioPlay>", spawn "cplay")
    , ("C-<XF86AudioPlay>", spawnToggleBluetooth)
    , ("S-<XF86AudioPlay>", spawnMeetingsRecorder)
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioLowerVolume>", spawn "/home/yann/system/scripts/changevolume down")
    , ("<XF86AudioRaiseVolume>", spawn "/home/yann/system/scripts/changevolume up")
    , ("<XF86MonBrightnessUp>", spawn "/home/yann/system/scripts/changebrightness up")
    , ("<XF86MonBrightnessDown>", spawn "/home/yann/system/scripts/changebrightness down")

  ------------------------------- Yeelight ----------------------------------------
  -- TODO: scripts use directory structure
    , ("S-<XF86AudioMute>",        spawn "~/system/bin/yeelight/controller.py toggle")
    , ("S-<XF86AudioLowerVolume>", spawn "~/system/bin/yeelight/controller.py down" )
    , ("S-<XF86AudioRaiseVolume>", spawn "~/system/bin/yeelight/controller.py up"   )
    , ("M1-<U>", spawn "xdotool getactivewindow windowmove --relative 0 -100"	    )
    , ("M1-<D>", spawn "xdotool getactivewindow windowmove --relative 0 +100"	    )
    , ("M1-<L>", spawn "xdotool getactivewindow windowmove --relative -- -100 0"    )
    , ("M1-<R>", spawn "xdotool getactivewindow windowmove --relative +100 0"	    )

  ------------------------------- DEBUG ----------------------------------------
    , ("M-r", spawn "notify-end DEBUG")
    ]

  ------------------------------- Prompts ----------------------------------------
    ++ [ ("M-p " ++ k, f dtXPConfig') | (k, f) <- promptList ]
    where a = 2

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
exclusiveSps :: ExclusiveScratchpads
exclusiveSps = mkXScratchpads [
      ("ticktick",      "ticktick",                             resource =? "ticktick"	) -- previously todo
    , ("anki" ,         "anki",                                 resource =? "anki"	)
    , ("keycombiner" ,  "keycombiner",                          resource =? "keycombiner")
    , ("cal" ,          "google-calendar-nativefier",           resource =? "googlecalendar-nativefier-e22938")
    , ("cutelog" ,      "cutelog",				resource =? "cutelog"	)
    , ("cmus",          "xterm -name cmus cmus",                resource =? "cmus"	)
    , ("nvtop",         "xterm -name nvtop nvtop",		resource =? "nvtop"	)
    , ("htop",          "xterm -bg black -name htop htop",      resource =? "htop"	)
    , ("hardinfo",      "hardinfo",				resource =? "hardinfo"	)
    , ("jshell",        "xterm -name jshell jshell",            resource =? "jshell"	)
    , ("pulse" ,        "xterm -name pulsemixer pulsemixer",    resource =? "pulsemixer")
    , ("python",        "xterm -fs 16 -name python ptpython",   resource =? "python"	)
    , ("R",             "xterm -fs 16 -name R R",               resource =? "R"		)
    , ("rambox",        "rambox",                               resource =? "rambox"	)
    , ("scala" ,        "xterm -name scala scala",              resource =? "scala"	)
    , ("spotify",       "spotify",                              resource =? "spotify"	)
    , ("stardict",      "stardict",                             resource =? "stardict")
    , ("todoist",       "todoist",                              resource =? "todoist"	)
    , ("timetrack",     "timetrack",                            resource =? "Timetrack"	)
    , ("xterm" ,        "xterm -name scratch",			  appName	 =? "scratch"	)
    , ("virt-manager" , "virt-manager",                         title    =? "Virtual Machine Manager") -- differentiate between the launcher and the VMs
    , ("youtrack" ,     "youtrack",                             resource =? "youtrack")
    , ("zotero",        "zotero",                               title    =? "Zotero"	)
    -- RationalRect: (x_start, y_start), (width, height)
    ] $ customFloating $ W.RationalRect 0.1 0.1 0.8 0.8



shiftMouse :: String -> X()
shiftMouse direction
    | direction == "left"  = spawn "exec xdotool mousemove_relative -- -1920 0"
    | direction == "right" = spawn "exec xdotool mousemove_relative    +1920 0"

mouseKeys :: [((ButtonMask, Button), Window -> X ())]
mouseKeys = [
-- button4 = scroll up, button3 = right click, button 5 = scroll down
     ((0, 6), \w -> moveToIndependent Prev True )
    -- , ((0, button3), \w -> spawn "exec xdotool key ctrl+return")
    , ((0, 7), \w -> moveToIndependent Next True )
    -- Middle mouse button: annotate label (0, 3) or button2
    , ((mod4Mask, button2), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    -- Remap Alt left click to a function spawn
    , ((mod1Mask, button1), \w -> focus w >> spawn "~/.pyenv/versions/data-collection-tools/bin/python ~/Projects/Entreprise/DataCollectionTools/bin/clipboard_append.py append-copy")
    -- Remap alt rigth click to a function
    , ((mod1Mask, button3), \w -> focus w >> spawn "~/.pyenv/versions/data-collection-tools/bin/python ~/Projects/Entreprise/DataCollectionTools/bin/clipboard_append.py append-paste")
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
strWorkspaces = ["1", "2", "3", "4", "5",
                 "6", "7", "8", "9"]

clickables = action strWorkspaces
    where action l = [ "<action=xdotool key super+" ++ n ++ ">" ++ ws ++ "</action>" |
                     (i, ws) <- zip strFKeys l, let n = i]


-- Spaghetti to separate monitors
physicalScreens :: X [Maybe ScreenId]
physicalScreens = withWindowSet $ \windowSet -> do
	let nScreens = length $ W.screens windowSet
	mapM (getScreen def . P) [0..nScreens]

getPhysicalScreen :: ScreenId -> X (Maybe PhysicalScreen)
getPhysicalScreen sid = do
	pscreens <- physicalScreens
	return $ Just sid `elemIndex` pscreens >>= \s -> Just (P s)

screenID :: X Char
screenID = withWindowSet $ \windowSet -> do
	let sid = W.screen (W.current windowSet)
	pscreen <- getPhysicalScreen sid
	return $ case pscreen of
				Just (P s) -> head (show s)
				_  -> 'r'


moveToIndependent :: Direction1D -> Bool -> X ()
moveToIndependent dir pad = do
    id <- screenID
    withWindowSet $ \ws -> do
        let numScreens = length $ W.screens ws
        if numScreens > 1
            then moveTo dir $ WSTagGroup $ xorgToIndependentScreenOrdering id
            else if pad
                then pure()
                else if dir == Next
                    then nextWS
                    else prevWS




-- XORG numbers screens like this: 0 1 2 (logical)
-- but X.L.IndependentScreens does that: 1 0 2 (somewhat less logical)
xorgToIndependentScreenOrdering :: Char -> Char
xorgToIndependentScreenOrdering c = case c of
  '0' -> '0'
  '1' -> '1'
  '2' -> '2'

-- End spaghetti

switchScreen :: Char -> X ()
switchScreen target = do
  current <- screenID
  case (current, target) of
    ('0', '1') -> nextScreen >> shiftMouse "right"
    ('0', '2') -> prevScreen >> shiftMouse "right" >> shiftMouse "right"
    ('1', '0') -> prevScreen >> shiftMouse "left"
    ('1', '2') -> nextScreen >> shiftMouse "right"
    ('2', '1') -> prevScreen >> shiftMouse "left"
    ('2', '0') -> nextScreen >> shiftMouse "left" >> shiftMouse "left"

shiftToScreen :: Char -> X ()
shiftToScreen target = do
  current <- screenID
  case (current, target) of
    ('0', '1') -> shiftNextScreen >> switchScreen target
    ('0', '2') -> shiftPrevScreen >> switchScreen target
    ('1', '0') -> shiftPrevScreen >> switchScreen target
    ('1', '2') -> shiftNextScreen >> switchScreen target
    ('2', '1') -> shiftPrevScreen >> switchScreen target
    ('2', '0') -> shiftNextScreen >> switchScreen target
