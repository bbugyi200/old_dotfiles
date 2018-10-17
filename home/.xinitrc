#!/bin/bash

source /etc/X11/xinit/xinitrc

numlockx &
dunst &
xbindkeys
clipster -d &
generate_xmobar_config &
exec xmonad > /var/tmp/xmonad.log
