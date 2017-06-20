#!/bin/bash

source /etc/X11/xinit/xinitrc

if [[ "$(hostname)" == "athena" ]]; then
    /usr/bin/dropbox &
fi

numlockx &
sxhkd &
clipmenud &
xbindkeys
exec bspwm
