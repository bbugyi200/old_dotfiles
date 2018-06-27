#!/bin/bash

# Added to fix okular icons
# Prereq Actions: Install 'qt5ct'; Run 'qt5ct' and select icon theme
[ "$XDG_CURRENT_DESKTOP" = "KDE" ] || [ "$XDG_CURRENT_DESKTOP" = "GNOME" ] || export QT_QPA_PLATFORMTHEME="qt5ct"

export PYTHONPATH=$PYTHONPATH:/home/bryan/Dropbox/scripts/pymodules
export XDG_CONFIG_HOME=/home/bryan/.config
export POWERLINE_DIRECTORY=/usr/lib/python3.6/site-packages
export PANEL_FIFO=/tmp/panel-fifo
export MATLABPATH=$HOME/.matlab
export SHELLCHECK_OPTS="-e SC1091 -e SC1117 -e SC2059 -e SC2129 -e SC2162"

# virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper_lazy.sh

# Latitude and Longitude coordinates for 417 Cripps Drive
export LATITUDE="39.997467N"
export LONGITUDE="74.778291W"

# Needed for ghc-mod
export cabal_helper_libexecdir=/home/bryan/.stack/global-project/.stack-work/install/x86_64-linux-tinfo6/lts-10.2/8.2.2/libexec/x86_64-linux-ghc-8.2.2/cabal-helper-0.8.0.2

# Enables executables installed with 'gem install' to run
export PATH="/usr/local/bin:$(ruby -e 'print Gem.user_dir')/bin:/home/bryan/.local/bin:/home/bryan/.cabal/bin:$PATH"

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -le 3 ] && [ "$(id -u)" != 0 ]; then
  exec startx
fi
