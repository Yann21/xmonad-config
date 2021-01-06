#!/usr/bin/bash
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
~/sys/trayer.sh &                                   # A system tray
aw-qt &                                             # Windows and time tracker (*)
xset r rate 200 25 &                                # Typematic delays
autokey-gtk &                                       # Rebind bash bindings in browser
/usr/bin/simplescreenrecorder \                     # Record screen (*)
    --start-hidden \
    --start-recording \
    --settingsfile=/home/yann/.ssr/settings_loy.conf \
    --no-systray &

# * data collection
