#!/bin/bash

###############################
#  Source Commands            #
###############################
function source_if_exists() {
    [[ -f "$1" ]] && source "$1"
}

source_if_exists /etc/profile
source_if_exists /usr/bin/virtualenvwrapper_lazy.sh


###############################
#  Environment Variables      #
###############################
# >>> Conveniance Variables
export DB="$HOME"/Sync
export DBB="$DB"/bin
export DBH="$DB"/home
export MOV=/media/bryan/zeus/media/Entertainment/Movies
export TV=/media/bryan/zeus/media/Entertainment/TV

# >>> Filesystem Paths
if [[ -f /etc/environment ]]; then 
    source <(sed 's/^\(.*\)="\(.*\)"/export \1="${\1}:\2"/' /etc/environment)
else
    export PATH="/usr/local/bin:/opt/bin:$PATH"
    export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
fi

export MATLABPATH="$HOME/.matlab"
export LIBRARY_PATH="/usr/local/lib:${LIBRARY_PATH}"
export MYPYPATH="$PYTHONPATH"

if [[ "$(id -u)" = 0 ]]; then
    export PATH="/root/.local/bin:$PATH"
fi

# >>> Miscellaneous
export BETTER_EXCEPTIONS=true  # enables python 'better_exceptions' library
export EDITOR="$(command -v vim)"  # I set this so the crontab would use vim for editing
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case'
export FZF_DEFAULT_OPTS='--reverse --height 40% --border'
export PATH="$HOME/.cargo/bin:$PATH"
export QT_QPA_PLATFORMTHEME="qt5ct"  # Fixes: missing okular icons
export RECENTLY_EDITED_FILES_LOG="$HOME"/Sync/var/recently_edited_files.log
export RIPGREP_CONFIG_PATH="$HOME"/.config/rgrc
export RUST_SRC_PATH="$HOME"/Sync/var/projects/rust/src
export SHELLCHECK_OPTS="-e SC1090 -e SC1091 -e SC1117 -e SC2001 -e SC2016 -e SC2059 -e SC2129 -e SC2155 -e SC2162"
export SHV_SHELL_HISTORY_ROOT="$HOME"/Sync/var/logs/shell-history
export WORKON_HOME=~/.virtualenvs
export XDG_CONFIG_HOME="$HOME"/.config

if [[ "$(uname -a)" == *"Darwin"* ]]; then
    export AT_WORK=true
    export BROWSER=/Applications/qutebrowser.app/Contents/MacOS/qutebrowser
    export GREP="ggrep"
    export LESS_OPTS=""
    export LOCATE="glocate"
    export LS="gls"
    export PATH=$PATH:/opt/local/bin
    export PYTHONPATH=$PYTHONPATH:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages
    export SED="gsed"
    export TERM="rxvt-256color"
else
    export BROWSER=qutebrowser
    export GREP="grep"
    export LESS_OPTS="-F"
    export LOCATE=locate
    export LS="ls"
    export SED="sed"
    export TERM="rxvt-unicode-256color"  # Fixes Mutt Background Issue (stays transparent) in TMUX

    rust_version="$(rustc --version | perl -anE 'print $F[1]')"
    export PATH=/opt/rust-bin-"${rust_version}"/bin:"$PATH"
fi

# Fixes "perl: warning: Setting locale failed." errors when using SSH to
# connect to bugyidesk at work.
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8


###########################
#  Start X Window System  #
###########################
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]] && [[ "$(id -u)" != 0 ]]; then
    exec startx
else
    if [[ "$(uname -a)" != *"Darwin"* ]]; then
        # Map Caps Lock to Ctrl
        #
        # WORKS EVEN WHEN WORKING FROM A VIRTUAL CONSOLE!
        map_file="$HOME"/.Caps2Ctrl.map

        sudo dumpkeys | head -1 > "${map_file}"
        printf "\n%s\n" "keycode 58 = Control" >> "${map_file}"
        sudo loadkeys "${map_file}"
    fi
fi

# MUST remain at bottom of file. Otherwise, other paths are prepended to $PATH
# somehow.
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.dynamic-colors/bin:$PATH"
export PATH="$HOME/.flamegraph:$PATH"

export PATH="$HOME/.poetry/bin:$PATH"
