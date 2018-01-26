#!/bin/bash

source /etc/X11/xinit/xinitrc

fswebcam "/home/bryan/login-sshots/%m.%d.%y-%H:%M:%S.jpg"
/usr/bin/dropbox &
numlockx &
clipmenud &
dunst &
xbindkeys
exec xmonad
