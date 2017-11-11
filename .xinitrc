#!/bin/bash

source /etc/X11/xinit/xinitrc

fswebcam "/home/bryan/login-sshots/%m.%d.%y-%H:%M:%S.jpg"
/usr/bin/dropbox &
numlockx &
# sxhkd &
clipmenud &
dunst &
xbindkeys
# exec bspwm
exec xmonad
