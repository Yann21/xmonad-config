#!/usr/bin/env bash
# Yann Hoffmann
# Equivalent of xinit for startup programs

redshift -c $HOME/.config/redshift/redshift.conf &  # Go easy on the eyes
$HOME/.screenlayout/arandrrc_1070.sh &              # Double monitor layout
nitrogen --restore &                                # Wallpaper
numlockx on &                                       # Numlock (previously in lightdm.conf)
ckb-next -b &                                       # Corsair bindings
picom -b &                                          # Compositor, transparency
udiskie &                                           # USB daemon
xscreensaver --no-splash -verbose &                 # Aerial screensaver
firefox &                                           # Firefox
aw-qt &                                             # Windows and time tracker (*)
xset r rate 190 25 &                                # Typematic delays
autokey-gtk &                                       # Rebind bash bindings in browser
#NetworkManager &                                   # Must be sudo WLAN
mousetrap -t 5 &                                    # Auto hide mouse after 5s
xmodmap -e "keycode 49 = Caps_Lock NoSymbol Caps_Lock" &
emacs &                                             # The one
copyq &                                             # Clipboard manager
#anki &
ulauncher &                                         # Dynamic menu
#intellij-idea-ultimate-edition &

#clipster -d &

# Record screen (*) we're oom
#/usr/bin/simplescreenrecorder \
    #--start-hidden \
    #--start-recording \
    #--settingsfile=/home/yann/.ssr/settings_loy.conf \
    #--no-systray &

# System tray
trayer \
    --edge bottom \
    --align center \
    --SetDockType true \
    --SetPartialStrut true \
    --expand true \
    --width 5 \
    --transparent true \
    --alpha 0 \
    --tint 0x0034435E \
    --heighttype pixel \
    --height 18 &


# * data collection
