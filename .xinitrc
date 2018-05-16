#!/bin/bash

source /etc/X11/xinit/xinitrc

fswebcam "/home/bryan/login-sshots/%m.%d.%y-%H:%M:%S.jpg"
/usr/bin/dropbox &
numlockx &
clipster -d &
dunst &
xbindkeys
fetchmail --syslog -d 900
khal-alarms &
exec xmonad
