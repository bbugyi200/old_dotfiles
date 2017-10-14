#!/bin/bash

# Added to fix okular icons
# Prereq Actions: Install 'qt5ct'; Run 'qt5ct' and select icon theme
[ "$XDG_CURRENT_DESKTOP" = "KDE" ] || [ "$XDG_CURRENT_DESKTOP" = "GNOME" ] || export QT_QPA_PLATFORMTHEME="qt5ct"

# export PYTHONPATH=$PYTHONPATH:/usr/lib/python3.6/site-packages
export XDG_CONFIG_HOME=/home/bryan/.config
export POWERLINE_DIRECTORY=/usr/lib/python3.6/site-packages
export PANEL_FIFO=/tmp/panel-fifo
export MATLABPATH=$HOME/.matlab

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -le 3 ] && [ "$(id -u)" != 0 ]; then
  exec startx
fi
