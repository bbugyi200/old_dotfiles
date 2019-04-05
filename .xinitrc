#!/bin/bash

source /etc/X11/xinit/xinitrc

numlockx &
dunst &
xbindkeys
clipster -d &
generate_xmobar_config &
/usr/bin/xflux -z 08060 -k 3000 &
exec xmonad 2>&1 | tee /var/tmp/xmonad.log
