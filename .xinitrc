#!/bin/bash

if [[ "$(uname -a)" != *"gentoo"* ]]; then
    exit 0
fi

source /etc/X11/xinit/xinitrc

numlockx &
dunst &
xbindkeys
clipster -d &
generate_xmobar_config &
/usr/bin/redshift -l 39.99:-74.78 -t 5000:3400 &
if watson_is_on; then xtaskidle -d & fi
exec xmonad 2>&1 | tee /var/tmp/xmonad.log
