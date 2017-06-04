#!/bin/bash

# Added to fix okular icons
# Prereq Actions: Install 'qt5ct'; Run 'qt5ct' and select icon theme
[ "$XDG_CURRENT_DESKTOP" = "KDE" ] || [ "$XDG_CURRENT_DESKTOP" = "GNOME" ] || export QT_QPA_PLATFORMTHEME="qt5ct"

export PATH=$PATH:/home/bryan/Dropbox/bin:/usr/local/bin:/home/bryan/Dropbox/config:/opt/anaconda/bin
export PYTHONPATH=/opt/anaconda/lib/python3.6/site-packages:$PYTHONPATH
export XDG_CONFIG_HOME=/home/bryan/.config
export PANEL_FIFO=/tmp/panel-fifo
export PANEL_HEIGHT=24
export PANEL_FONT="-*-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
export PANEL_WM_NAME=bspwm_panel
export POWERLINE_DIRECTORY=/usr/lib/python3.6/site-packages

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -le 3 ]; then
  exec startx
fi
