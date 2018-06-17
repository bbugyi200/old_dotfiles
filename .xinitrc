#!/bin/bash

source /etc/X11/xinit/xinitrc

fswebcam "/home/bryan/login-sshots/%m.%d.%y-%H:%M:%S.jpg"
/usr/bin/dropbox &
numlockx &
dunst &
xbindkeys
fetchmail --syslog -d 900
khal-alarms &
clipster -d &
exec xmonad
