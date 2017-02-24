# ----------------------------------------------------------------------------
# ----------------------------------- ALIASES --------------------------------
alias sql="rlwrap sqlite3"

alias budget="cd ~/Dropbox/Budget/ && ./IntelliBudget > /dev/null & disown"

alias mutt-edu="mutt -e 'source ~/.mutt/hooks/bryan_bugyi.mymail.rcbc'"

# -------------------------------- FUNCTIONS ---------------------------------
function tm() {
    if [ $1 == 'ls' ]
    then 
        tmux ls
    elif [ $# -eq 0 ]
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
    FILE=~/Dropbox/notes/sbreak.txt
    if [ "$1" == '--clear' -o "$1" == '-c' ]; then 
        sed -i 's/\[X\]/\[\]/g' $FILE
        sed -i '/([X]\?)/d' $FILE
    fi

    if [ "$2" == '--silent' -o "$2" == '-s' ]; then
        : # NOOP
    else
        vim $FILE
    fi
}


function getPWD() {
    echo $(pwd | sed 's/\/home\/[Bb]ryan/~/' | cut -c -55) 
}

# ETERNAL BASH HISTORY
# https://spin.atomicobject.com/2016/05/28/log-bash-history/
function log_bash_history() { 
    if [ "$(id -u)" -ne 0 ]; 
    then printf "%-20s%-25s%-60s%s\n" "$(hostname)" "$(date '+%Y-%m-%d.%H:%M:%S')" "$(getPWD)" "$(history 1 | cut -c 8-)" >> ~/Dropbox/logs/bash-history.log;
    fi; 
}

function hgrep() {
    if [ "$1" == '--local' -o "$1" == '-L' ]
    then
        cat ~/Dropbox/logs/bash-history.log | nl -s ' ' | grep -e " $(getPWD) " | grep -e "$(hostname)" | tr -s ' '| cut -d' ' -f 2,6- | grep -e "$2"
    elif [ "$1" == '--verbose' -o "$1" == '-V' ]
    then
        cat ~/Dropbox/logs/bash-history.log | nl | grep -e "$2"
    else
        cat ~/Dropbox/logs/bash-history.log | nl -s ' ' | grep -e "$(hostname)" | tr -s ' ' | cut -d' ' -f 2,6- | grep -e "$1"
    fi
}

function hview() {
    if [ $# -eq 0 ]
    then 
        vim ~/Dropbox/logs/bash-history.log
    else
        vim +$1 ~/Dropbox/logs/bash-history.log
    fi
}

# Enables vman command from the terminal
vman() {
  vim -c "SuperMan $*"

  if [ "$?" != "0" ]; then
    echo "No manual entry for $*"
  fi
}

# ------------------------------ EXPORTS -------------------------------------
# I set this so the crontab would use vim for editing
export EDITOR=$(which vim)

export PROMPT_COMMAND="log_bash_history; $PROMPT_COMMAND"

# Needed for powerline to work
export TERM="screen-256color"

# ------------------------------- MISC ---------------------------------------
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $POWERLINE_DIRECTORY/powerline/bindings/bash/powerline.sh
