#!/bin/bash

source /etc/X11/xinit/xinitrc

numlockx &
sxhkd &
clipmenud &
xbindkeys
exec bspwm
