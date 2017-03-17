# ----------------------------------------------------------------------------
# ------------------------------ SOURCES -------------------------------------
source localAlias

# ----------------------------------- ALIASES --------------------------------
alias sql="rlwrap sqlite3"

alias budget="cd ~/Dropbox/Budget/ && ./IntelliBudget > /dev/null & disown"

alias mutt-edu="mutt -e 'source ~/.mutt/hooks/bryan_bugyi.mymail.rcbc'"

alias Exp="nautilus ."

alias tree="tree -I \"__pycache__\""

alias lls="ls -1"

# -------------------------------- FUNCTIONS ---------------------------------

precmd() { eval "$PROMPT_COMMAND" }

function tm() {
    if [ "$1" = 'ls' ]
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

function ccd {
    case "$1" in
        ww)
            cd ~/Dropbox/college_studies/UGR/WW;;
        config)
            cd $CONFIG;;
        flasky)
            cd ~/My_Projects/Flasky;;
        budget)
            cd ~/Dropbox/Python_Projects/IntelliBudget;;
        notes)
            cd ~/Dropbox/notes;;
        data)
            cd ~/Dropbox/Learning_Environments/DataScience;;
    esac
}

function sbreak() {
    FILE=~/Dropbox/notes/sbreak.txt
    if [ "$1" = '--clear' -o "$1" = '-c' ]; then 
        sed -i 's/\[X\]/\[\]/g' $FILE
        sed -i '/([X]\?)/d' $FILE
    fi

    if [ "$2" = '--silent' -o "$2" = '-s' ]; then
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
    then printf "%-20s%-25s%-60s%s\n" "$(hostname)" "$(date '+%Y-%m-%d.%H:%M:%S')" "$(getPWD)" "$(fc -ln -1)" >> ~/Dropbox/logs/bash-history.log;
    fi; 
}

function hgrep() {
    if [ "$1" = '--local' -o "$1" = '-l' ]
    then
        cat ~/Dropbox/logs/bash-history.log | nl -s ' ' | grep -e " $(getPWD) " | grep -e "$(hostname)" | tr -s ' '| cut -d' ' -f 2,6- | grep -e "$2"
    elif [ "$1" = '--verbose' -o "$1" = '-v' ]
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

# orig_command_not_found ---> command_not_found_handle
# http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
# eval "$(echo orig_command_not_found(); declare -f command_not_found_handler | tail -n +2)"
command_not_found_handler() {
    GREP=$(grep -s "^$1$SEP" "./.localaliases")
    if LocalAlias $1 "$GREP" "${@:2}"; then
          :
#         echo
#         orig_command_not_found "$1"
    fi
}

# ------------------------------ EXPORTS -------------------------------------
# I set this so the crontab would use vim for editing
export EDITOR=$(which vim)

export PROMPT_COMMAND="log_bash_history; $PROMPT_COMMAND"

# Needed for powerline to work
# export TERM="screen-256color"

# ------------------------------- MISC ---------------------------------------
# powerline-daemon -q
# POWERLINE_BASH_CONTINUATION=1
# POWERLINE_BASH_SELECT=1
# . $POWERLINE_DIRECTORY/powerline/bindings/bash/powerline.sh

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon
