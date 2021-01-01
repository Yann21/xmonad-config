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
--m, altMask :: KeyMask
--altMask = mod1Mask
--m       = mod4Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
    -- XF86XK_AudioLowerVolume, XF86XK_AudioRaiseVolume, XF86XK_AudioMute
    -- XF86XK_AudioPlay, XF86XK_AudioStop, XF86XK_AudioPrev, XF86XK_AudioNext
      ((0,                 0x1008FF11), spawn "pulsemixer --change-volume -15")
    , ((0,                 0x1008FF13), spawn "pulsemixer --change-volume +15")
    , ((0,                 0x1008FF12), spawn "pulsemixer --toggle-mute"      )
    , ((0,                 0x1008FF14), spawn "cplay"                         )
    , ((0,                 0x1008FF15), spawn "xterm"                         )
    , ((0,                 0x1008FF16), spawn "xterm"                         )
    , ((0,                 0x1008FF17), passPrompt dtXPConfig'                )
    ]


    -- Debug
    --, ((m .|. controlMask, xK_t), spawn $ "notify-send " ++ (get (withWindowSet (pure . W.currentTag))))
    --, ((m .|. controlMask, xK_t), spawn $ "notify-send " ++ (logCurrent))
    --, ((m .|. controlMask, xK_t), spawn $ "notify-send " ++ (show $ gets (W.screen . W.current)))
    --, ( (m, xK_p), spawn $ "notify-send " ++ (show $ gets (W.screen . W.current))) -- $ withScreens 1 clickables))

--funk = do
----    mon <- W.screen . W.current
--    spawn "notify-send a" -- ++ mon


emacsKeys :: [(String, X())]
emacsKeys = [
  ------------------------------- Programs ----------------------------------------
      ("M4-S-<Return>", shellPrompt dtXPConfig)
    , ("C-M1-t", spawn "xterm"          )
    , ("C-M1-a", spawn "atom"           )
    , ("C-M1-c", spawn "calibre"        )
    , ("C-M1-d", spawn "dropbox"        )
    , ("C-M1-e", spawn "evince"         )
    , ("C-M1-f", spawn  "firefox"       )
    , ("C-M1-S-f", spawn  "firefox-developer-edition" )
    , ("C-M1-i", spawn "idea"           )
    , ("C-M1-j", spawn "joplin-desktop" )
    , ("C-M1-l", spawn "libreoffice"    )
    , ("C-M1-p", spawn "pycharm"        )
    , ("C-M1-p", spawn "rambox"         )
    , ("C-M1-s", spawn "slack"          )
    , ("C-M1-v", spawn "virtualbox"     )
    , ("C-M1-z", spawn "zotero"         )


  ------------------------------- Scratchpads ----------------------------------------
    , ("C-M-c", scratchpadAction exclusiveSps "cmus"      )
    , ("C-M-d", scratchpadAction exclusiveSps "cal"       )
    , ("C-M-g", scratchpadAction exclusiveSps "ghci"      )
    , ("C-M-h", scratchpadAction exclusiveSps "htop"      )
    , ("C-M-n", scratchpadAction exclusiveSps "mailspring")
    , ("C-M-p", scratchpadAction exclusiveSps "python"    )
    , ("C-M-m", scratchpadAction exclusiveSps "rambox"    )
    , ("C-M-s", scratchpadAction exclusiveSps "scala"     )
    , ("C-M-t", scratchpadAction exclusiveSps "trello"    )
    , ("C-M-w", scratchpadAction exclusiveSps "todoist"   )
    , ("C-M-v", scratchpadAction exclusiveSps "pulse"     )


  ------------------------------- Layouts ----------------------------------------
    --, ("M-i"  , moveTo Next (WSIs $ return (('0' `elem`) . W.tag)) )
    --, ("M-u"  , moveTo Prev (WSIs $ return (('0' `elem`) . W.tag)) )
    , ("M-i"  , moveTo Next $ WSTagGroup '0'  )
    , ("M-u"  , moveTo Prev $ WSTagGroup '0'  )
    , ("M-S-i"  , moveTo Next $ WSTagGroup '1')
    , ("M-S-u"  , moveTo Prev $ WSTagGroup '1')
    , ("M-k", B.focusDown           )
    , ("M-j", B.focusUp             )
    , ("M-h", sendMessage Shrink    )
    , ("M-l", sendMessage Expand    )
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-S-<Tab>", sendMessage FirstLayout)
    , ("M-<Space>", sendMessage (MT.Toggle NBFULL )
         >> sendMessage ToggleStruts
         >> spawn "sleep 0.3 exec xdotool key f" ) --TODO Remove

    , ("M-a"  , prevScreen >> shiftMouse "left" )
    , ("M-e"  , nextScreen >> shiftMouse "right")
    , ("M-z"  , shiftNextScreen >> nextScreen   ) -- TODO: Improve
    , ("M-S-k", windows W.swapDown              )
    , ("M-S-j", windows W.swapUp                )
    , ("M-S-c", kill1                           )
    , ("M-S-a", killAll                         )
    , ("M-t t", treeselectAction tsDefaultConfig)
    , ("M-s s", sshTreeselectAction tsDefaultConfig)


  ------------------------------- Functions ----------------------------------------
    , ("M-<KP_F4>",     spawnShutdown )
    , ("M-<KP_F5>",     spawnRestart  )
    , ("<Print>"  ,     spawnScrot    )
    , ("S-<Print>",     spawnRectScrot)
    , ("M-S-r"    ,     spawnRecompile)
    ]

  ------------------------------- Prompts ----------------------------------------
    ++ [ ("M-p " ++ k, f dtXPConfig') | (k, f) <- promptList ]
    
    
  ------------------------------- DEBUG ----------------------------------------
    ++ [
      ("M-r", spawn $ "notify-send " ++ show (marshall 0 $ head clickables))
--  , ("M-r", spawn $ "notify-send var")
    , ("C-M-r", spawn $ "notify-send variation")
    ]
-- X (Maybe String)
--funk :: Maybe String
funk = do 
  date "%a %b %d"


shiftMouse :: String -> X()
shiftMouse direction
    | direction == "left"  = spawn "exec xdotool mousemove_relative -- -1920 0"
    | direction == "right" = spawn "exec xdotool mousemove_relative    +1920 0"

mouseKeys :: [((ButtonMask, Button), Window -> X ())]
mouseKeys = [
      ((0, 6), \w -> moveTo Prev (WSIs $ return (('0' `elem`) . W.tag)) )
    , ((0, 7), \w -> moveTo Next (WSIs $ return (('0' `elem`) . W.tag)) )
    ]


------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
confKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ [
    ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) azertyKeys
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]

    ] ++ [ -- For xdotooling clickables
    ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) fKeys
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

azertyKeys, fKeys :: [KeySym]
azertyKeys = [0x26, 0xe9, 0x22, 0x27, 0x28, 0x2d, 0xe8, 0x5f, 0xe7]
fKeys      = [xK_F1 .. xK_F9]

strWorkspaces, strFKeys, clickables :: [String]
strFKeys      = ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"]
strWorkspaces = ["chess", "dev", "job", "**", "***", "**", "lang", "alf", "dump"]
--strWorkspaces = ["ide", "dev", "job", "edu", "vid", "org", "lang", "pm", "dump"]
--strWorkspaces = ["ide", "dev", "cours", "misc", "*", "misc", "lang", "xmon", "sys"]

clickables = action strWorkspaces
    where action l = [ "<action=xdotool key super+" ++ n ++ ">" ++ ws ++ "</action>" |
                     (i, ws) <- zip strFKeys l, let n = i]
