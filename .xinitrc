#!/bin/bash

source /etc/X11/xinit/xinitrc

numlockx &
dunst &
xbindkeys
clipster -d &
generate_xmobar_config &
# /usr/bin/redshift -l 39.99:-74.78 -t 4000:3400 &
exec xmonad 2>&1 | tee /var/tmp/xmonad.log
