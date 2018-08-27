#!/bin/bash

source /etc/X11/xinit/xinitrc

numlockx &
dunst &
xbindkeys
fetchmail --syslog -d 900
clipster -d &
exec xmonad
