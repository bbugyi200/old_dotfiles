#!/bin/bash
# [[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

source /etc/X11/xinit/xinitrc

numlockx &
sxhkd &
clipmenud &
xbindkeys
exec bspwm
