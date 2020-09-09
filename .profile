#!/bin/bash

###############################
#  Functions                  #
###############################
function source_if_exists() {
    [[ -f "$1" ]] && source "$1"
}

function insert_path() {
    local path_="$1"; shift
    local P="$1"; shift

    if _is_in_path "${path_}" "${P}"; then
        local new_path=
        for p in $(echo "${path_}" | tr ":" "\n"); do
            if [[ "${p}" == "${P}" ]]; then
                continue
            fi

            if [[ -n "${new_path}" ]]; then
                new_path="${new_path}":"${p}"
            else
                new_path="${p}"
            fi
        done

        path_="${new_path}"
    fi

    echo "${P}":"${path_}"
}

function dedup_path() {
    local path_="$1"; shift
    local new_path=

    for P in $(echo "${path_}" | tr ":" "\n"); do
        if ! _is_in_path "${new_path}" "${P}"; then
            if [[ -n "${new_path}" ]]; then
                new_path="${new_path}":"${P}"
            else
                new_path="${P}"
            fi
        fi
    done

    echo "${new_path}"
}

function _is_in_path() {
    local path_="$1"; shift
    local P="$1"; shift
    if [[ "${path_}" == "${P}" ]]; then
        return 0
    fi

    if [[ "${path_}" == "${P}:"* ]]; then
        return 0
    fi

    if [[ "${path_}" == *":${P}" ]]; then
        return 0
    fi

    if [[ "${path_}" == *":${P}:"* ]]; then
        return 0
    fi

    return 1
}


###############################
#  Source Commands            #
###############################
source_if_exists /etc/profile
source_if_exists /usr/bin/virtualenvwrapper_lazy.sh


###############################
#  Environment Variables      #
###############################
# >>> Conveniance Variables
export DB="$HOME"/Sync
export DBB="$DB"/bin
export DBH="$DB"/home
export MOV=/mnt/hercules/plex/Movies
export TV=/mnt/hercules/plex/TV

# >>> Filesystem Paths
if [[ -f /etc/environment ]]; then 
    source <(sed 's/^\(.*\)="\(.*\)"/export \1="${\1}:\2"/' /etc/environment)
else
    export PATH="$(insert_path "${PATH}" "/usr/local/bin")"
    export PATH="$(insert_path "${PATH}" "/usr/local/opt/gnu-getopt/bin")"
    export PATH="$(insert_path "${PATH}" "${HOME}"/.cargo/bin)"
    export PATH="$(insert_path "${PATH}" "${HOME}"/.poetry/bin)"
fi

export MATLABPATH="$HOME/.matlab"

if [[ "$(id -u)" = 0 ]]; then
    export PATH="$(insert_path "${PATH}" "/root/.local/bin")"
fi

# >>> Miscellaneous
export BB=bbugyi.ddns.net
export EDITOR="$(command -v vim)"  # I set this so the crontab would use vim for editing
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case'
export FZF_DEFAULT_OPTS='--reverse --height 40% --border'
M="$(printf "\u2709")"
export LESS="${LESS}QR"
export MAILPATH="/var/mail/bryan? ${M} ${M} ${M} NEW MAIL IN /var/mail/bryan!!! ${M} ${M} ${M}"
export PAGER="less"
export QT_QPA_PLATFORMTHEME="qt5ct"  # Fixes: missing okular icons
export RECENTLY_EDITED_FILES_LOG="$HOME"/Sync/var/recently_edited_files.log
export RIPGREP_CONFIG_PATH="$HOME"/.config/rgrc
export RUST_SRC_PATH="$HOME"/Sync/var/projects/rust/src
export SHELLCHECK_OPTS="-e SC1090 -e SC1091 -e SC1117 -e SC2001 -e SC2016 -e SC2046 -e SC2059 -e SC2129 -e SC2155 -e SC2162"
export SHV_SHELL_HISTORY_ROOT="$HOME"/Sync/var/logs/shell-history
export WORKON_HOME=~/.virtualenvs
export XDG_CONFIG_HOME="$HOME"/.config

if [[ "$(uname -a)" == *"Darwin"* ]]; then
    export AT_WORK=true
    export BROWSER=/Applications/qutebrowser.app/Contents/MacOS/qutebrowser
    export GREP="ggrep"
    export LS="gls"
    export PATH=$PATH:/opt/local/bin
    export PYTHONPATH="$(insert_path "${PYTHONPATH}" /opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages)"
    export SED="gsed"
    export TERM="rxvt-256color"

    source_if_exists /Users/bbugyi/.private.sh
else
    export BROWSER=qutebrowser
    export GREP="grep"
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
export PATH="$(insert_path "${PATH}" "$HOME/.local/bin")"
export PATH="$(insert_path "${PATH}" "$HOME/.dynamic-colors/bin")"
export PATH="$(insert_path "${PATH}" "$HOME/.flamegraph")"
export PATH="$(insert_path "${PATH}" "${HOME}"/.tmp/bin)"

export PYTHONPATH="$(insert_path "${PYTHONPATH}" "${HOME}"/.local/bin)"

export LIBRARY_PATH="$(dedup_path "${LIBRARY_PATH}")"
export PATH="$(dedup_path "${PATH}")"
export PYTHONPATH="$(dedup_path "${PYTHONPATH}")"
export MYPYPATH="${PYTHONPATH}"
export LESS="$(python -c "import sys; print(''.join(set('${LESS}')))")"

export PATH="$HOME/.cargo/bin:$PATH"
