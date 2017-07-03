#!/bin/bash

source /etc/X11/xinit/xinitrc

/usr/bin/dropbox &
numlockx &
sxhkd &
clipmenud &
xbindkeys
exec bspwm
