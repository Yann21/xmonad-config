{-# OPTIONS_GHC -Wno-all -Wno-name-shadowing -fno-warn-unused-binds #-}
module Modules.MyTreeSelect where

import Data.Tree
import XMonad
import qualified Data.Map as M
import qualified XMonad.Actions.TreeSelect as TS

------------------------------------------------------------------------
-- TREESELECT
------------------------------------------------------------------------
editConfig e = "xterm -e vim " ++ e

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
    [ Node (TS.TSNode "+ Config Files"  "config files quick access" (return ()))
        [ Node (TS.TSNode "xmonad.hs"   "xmonad config"    (spawn (editConfig "~/.xmonad/xmonad.hs"      ))) []
        , Node (TS.TSNode "zshrc"      "zsh config"      (spawn (editConfig "~/.zshrc"                ))) []
        , Node (TS.TSNode "bashrc"      "bash config"      (spawn (editConfig "~/.bashrc"                ))) []
        , Node (TS.TSNode "vimrc"       "vim config"       (spawn (editConfig "~/.vim/vimrc"             ))) []
        , Node (TS.TSNode "xmobarrc"    "xmobar config"    (spawn (editConfig "~/.config/xmobar/xmobarrc"))) []
        , Node (TS.TSNode "picom"       "picom config"     (spawn (editConfig "~/.config/picom/picom.conf"))) []
        , Node (TS.TSNode "xresources"  "xresconfig"       (spawn (editConfig "~/.Xresources"            ))) []
        ]
    , Node (TS.TSNode "+ XMonad"        "window manager commands" (return ()))
       [ Node (TS.TSNode "Recompile"    "Recompile XMonad"        (spawn "xmonad --recompile && xmonad --restart")) []
       , Node (TS.TSNode "Restart"      "Restart XMonad"          (spawn "xmonad --restart")) []
       --, Node (TS.TSNode "Quit"         "Restart XMonad"          (io exitSuccess)) []
       ]
    ]


sshConnect server = "xterm -e ssh " ++ server
-- saves the commands entered during the ssh session
traceSSH server = "xterm -e script -c 'ssh " ++ server ++ "' ~/sys/logs/" ++ server ++ "/$(date -u +'%m:%d.%H:%M:%S')"
vpnConnect vpnConfig  = "xterm -e sudo openvpn ~/.ssh/vpn/" ++ vpnConfig
tunnel          = "xterm -e ssh -L 3333:git.talkwalker:22 dev@10.103.2.179"

sshTreeselectAction a = TS.treeselectAction a
    [ Node (TS.TSNode "+ Talkwalker"   "Work related access" (return ()))
        [ Node (TS.TSNode "vpn"      "Work VPN"        (spawn (vpnConnect "y.hoffmann.ovpn" ))) []
        , Node (TS.TSNode "tunnel"   "Work git access" (spawn tunnel)) []
        , Node (TS.TSNode "ssh"      "Work ssh"        (spawn (sshConnect "work"  ))) []
        ]
    , Node (TS.TSNode "+ WAN Machines"      "Quick access to servers" (return ()))
        [ Node (TS.TSNode "minsky"          "[!] UL - Minsky Production [VPN]" (spawn (traceSSH "minsky"  ))) []
        , Node (TS.TSNode "iris-cluster"    "ULHPC - yhoffmann"            (spawn (sshConnect "iris-cluster" ))) []
        , Node (TS.TSNode "vpn"             "London AWS OpenVPN Server"        (spawn (sshConnect "vpn"))) []
        ]
    , Node (TS.TSNode "+ LAN Machines"  "Quick access to local machines" (return ()))
        [ Node (TS.TSNode "pi"     "Erdgeschoss raspberry pi"   (spawn (sshConnect "pi"      ))) []
        , Node (TS.TSNode "backup" "Backup Dell Laptop"         (spawn (sshConnect "backup"  ))) []
        , Node (TS.TSNode "claude" "Claude's Mac"               (spawn (sshConnect "claude"  ))) []
        , Node (TS.TSNode "jmh"    "Windows 10 JMH"             (spawn (sshConnect "jmh"     ))) []
        ]
    ]

--myTreeNavigation :: M.Map (KeyMask, KeySym) (TreeSelect a (Maybe a))
myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel          )
    , ((mod1Mask, xK_Escape), TS.cancel     )        -- type casting
    , ((0, xK_space ),   TS.select          )
    , ((0, xK_k     ),   TS.movePrev        )
    , ((0, xK_j     ),   TS.moveNext        )
    , ((0, xK_h     ),   TS.moveParent      )
    , ((0, xK_l     ),   TS.moveChild       )
    , ((0, xK_u     ),   TS.moveHistBack    )
    , ((0, xK_i     ),   TS.moveHistForward )
    ]

