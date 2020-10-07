#!/bin/sh

# source /etc/X11/xinit/xinitrc

numlockx
dunst &
xbindkeys
xmodmap -v /home/bryan/.Xmodmap
clipster -d &
generate_xmobar_config &
# /usr/bin/redshift -l 39.99:-74.78 -t 5000:3400 &
start_sync &
rfserver &
fswebcam /home/bryan/img/login/"$(hostname)"/"$(date +"%Y-%m-%d-%H.%M.%S.jpg")"
if watson_is_on; then xtaskidle -d & fi
exec x-window-manager
