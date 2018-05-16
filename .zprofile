#!/bin/bash

# Added to fix okular icons
# Prereq Actions: Install 'qt5ct'; Run 'qt5ct' and select icon theme
[ "$XDG_CURRENT_DESKTOP" = "KDE" ] || [ "$XDG_CURRENT_DESKTOP" = "GNOME" ] || export QT_QPA_PLATFORMTHEME="qt5ct"

export PYTHONPATH=$PYTHONPATH:/home/bryan/Dropbox/scripts/pymodules
export XDG_CONFIG_HOME=/home/bryan/.config
export POWERLINE_DIRECTORY=/usr/lib/python3.6/site-packages
export PANEL_FIFO=/tmp/panel-fifo
export MATLABPATH=$HOME/.matlab

# virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper_lazy.sh

# Enables executables installed with 'gem install' to run
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -le 3 ] && [ "$(id -u)" != 0 ]; then
  exec startx
fi
