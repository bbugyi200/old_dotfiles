# ----------------------------------------------------------------------------
# I set this so the crontab would use vim for editing
export EDITOR=$(which vim)

# ----------------------------------- ALIASES --------------------------------
alias sql="rlwrap sqlite3"

alias hview="vim ~/Dropbox/logs/bash-history.log"

alias budget="cd ~/Dropbox/Budget/ && ./IntelliBudget > /dev/null & disown"

# -------------------------------- FUNCTIONS ---------------------------------
function tm() {
    if [ $# -eq 0 ]
    then
        tmux -2
    else
        tmux -2 new -s $1
    fi
}

function ta() {
    if [ $# -eq 0 ]
    then 
        tmux -2 attach
    else
        tmux -2 attach -t $1
    fi
}

function sbreak() {
    if [ "$1" == '--clear' ]
    then 
        cp ~/Dropbox/notes/sbreak/template.txt ~/Dropbox/notes/sbreak/sbreak.txt
    fi

    vim ~/Dropbox/notes/sbreak/sbreak.txt
}


function getPWD() {
    echo $(pwd | sed 's/\/home\/bryan/~/' | cut -c -55) 
}


function hgrep() {
    if [ "$1" == '--local' -o "$1" == '-L' ]
    then
        cat ~/Dropbox/logs/bash-history.log | nl -s ' ' | grep -e " $(getPWD) " | tr -s ' '| cut -d' ' -f 2,6- | grep -e "$2"
    else
        cat ~/Dropbox/logs/bash-history.log | tr -s ' ' | cut -d' ' -f 4- | nl | grep -e "$1"
    fi
}

# ------------------------------- POWERLINE ----------------------------------
export TERM="screen-256color"
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $POWERLINE_DIRECTORY/powerline/bindings/bash/powerline.sh

# ------------------------------- VMAN ---------------------------------------
# Enables vman command from the terminal
vman() {
  vim -c "SuperMan $*"

  if [ "$?" != "0" ]; then
    echo "No manual entry for $*"
  fi
}

# -------------------------- ETERNAL BASH HISTORY ----------------------------

# https://spin.atomicobject.com/2016/05/28/log-bash-history/
function log_bash_history() { 
    if [ "$(id -u)" -ne 0 ]; 
    then printf "%-20s%-25s%-60s%s\n" "$(hostname)" "$(date '+%Y-%m-%d.%H:%M:%S')" "$(getPWD)" "$(history 1 | cut -c 8-)" >> ~/Dropbox/logs/bash-history.log;
    fi; 
}

export PROMPT_COMMAND="log_bash_history; $PROMPT_COMMAND"
