#!/bin/bash

source /etc/X11/xinit/xinitrc

numlockx &
dunst &
xbindkeys
clipster -d &
exec xmonad > /var/tmp/xmonad.log
