#!/usr/bin/bash
# Equivalent of xinit for startup programs
redshift -c ~/.config/redshift/redshift.conf &
~/.screenlayout/arandrrc_nvidia.sh &
nitrogen --restore &
numlockx on &
ckb-next -b &
picom -b &
udiskie &
xscreensaver &
firefox &
~/sys/trayer.sh &
aw-qt &
xset r rate 200 25 &
/usr/bin/simplescreenrecorder \
    --start-hidden \
    --start-recording \
    --settingsfile=/home/yann/.ssr/settings_loy.conf \
    --no-systray &
