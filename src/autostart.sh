#!/usr/bin/env bash
# Yann Hoffmann
# Equivalent of xinit for startup programs

redshift -c ~/.config/redshift/redshift.conf &      # Go easy on the eyes
$HOME/.screenlayout/arandrrc_nvidia.sh &            # Double monitor layout
nitrogen --restore &                                # Wallpaper
numlockx on &                                       # Numlock (previously in lightdm.conf)
ckb-next -b &                                       # Corsair bindings
picom -b &                                          # Compositor, transparency
udiskie &                                           # USB daemon
xscreensaver &                                      # Aerial screensaver
firefox &                                           # Firefox
aw-qt &                                             # Windows and time tracker (*)
xset r rate 200 25 &                                # Typematic delays
autokey-gtk &                                       # Rebind bash bindings in browser
NetworkManager &                                    # WLAN

# Record screen (*)
/usr/bin/simplescreenrecorder \
    --start-hidden \
    --start-recording \
    --settingsfile=/home/yann/.ssr/settings_loy.conf \
    --no-systray &

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
