#!/usr/bin/env bash
# Yann Hoffmann
# Equivalent of xinit for startup programs
# or $XDG_CONFIG_HOME/autostart/*.desktop

redshift -c $HOME/.config/redshift/redshift.conf &  # Go easy on the eyes
$HOME/.screenlayout/arandrrc.sh &                   # Double monitor layout
nitrogen --restore &                                # Wallpaper
numlockx on &                                       # Numlock (previously in lightdm.conf)
picom -b &                                          # Compositor, transparency
xset r rate 190 25 &                                # Typematic delays
autokey-gtk &                                       # Rebinds bash bindings in browser
mousetrap -t 5 &                                    # Auto hide mouse after 5s
xmodmap -e "keycode 49 = Caps_Lock NoSymbol Caps_Lock" & # Makes ^2 key act as caps lock (autokey most likely not working)
copyq &                                             # Clipboard manager
ulauncher &                                         # Dynamic menu
firefox &                                           # Firefox
# vim &                                               # Self Q&A
# emacs &                                             # The one

# Synchronize important directories between machines
#gitwatch -r origin -b main $HOME/Org &
#gitwatch -r origin -b master $HOME/.xmonad &
#gitwatch -r origin -b master $HOME/.dotfiles &
#gitwatch -r origin -b master $HOME/system &


# @Home
if ! [[ $(uname --all) =~ "Ubuntu" ]] then
    ckb-next -b &                                       # Corsair bindings
    $HOME/Code/tools/Pycharm2019/pycharm-2019.3.4/bin/pycharm.sh &
    udiskie &                                           # USB daemon
    aw-server &                                         # Windows and time tracker (*)
    xscreensaver --no-splash -verbose &                 # Aerial screensaver
    noip2 &
fi

# Record screen (*) we're oom
#/usr/bin/simplescreenrecorder \
    #--start-hidden \
    #--start-recording \
    #--settingsfile=/home/yann/.ssr/settings_loy.conf \
    #--no-systray &

# System tray
#trayer \
    #--edge bottom \
    #--align center \
    #--SetDockType true \
    #--SetPartialStrut true \
    #--expand true \
    #--width 5 \
    #--transparent true \
    #--alpha 0 \
    #--tint 0x0034435E \
    #--heighttype pixel \
    #--height 18 &

# * data collection
