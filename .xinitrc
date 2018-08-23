#!/bin/bash

source /etc/X11/xinit/xinitrc

# fswebcam "/home/bryan/login-sshots/%m.%d.%y-%H:%M:%S.jpg"
numlockx &
dunst &
xbindkeys
# fetchmail --syslog -d 900
clipster -d &
exec xmonad
