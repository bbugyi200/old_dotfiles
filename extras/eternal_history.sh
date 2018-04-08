# ---------- ETERNAL BASH HISTORY
# https://spin.atomicobject.com/2016/05/28/log-bash-history/
LOGFILE="/home/bryan/Dropbox/logs/$(hostname)-cmd-history.log"
log_bash_history() { 
    if [ "$(id -u)" -ne 0 ]; then
        printf "%-20s%-25s%-60s%s\n" "$(hostname)" "$(date '+%Y-%m-%d.%H:%M:%S')" "$(pwd | sed 's/\/home\/[Bb]ryan/~/' | cut -c -55)" "$(fc -ln -1)" >> $LOGFILE;
    fi; 
}

hgrep() {
    if [ "$1" = '--local' -o "$1" = '-l' ]
    then
        cat $LOGFILE | nl -s ' ' | grep -a -e " $(getPWD) " | grep -a -e "$(hostname)" | tr -s ' '| cut -d' ' -f 2,6- | grep -a -e "$2"
    elif [ "$1" = '--verbose' -o "$1" = '-v' ]
    then
        cat ~/Dropbox/logs/athena-cmd-history.log ~/Dropbox/logs/aphrodite-cmd-history.log | nl | grep -a -e "$2"
    else
        cat $LOGFILE | nl -s ' ' | grep -a -e "$(hostname)" | tr -s ' ' | cut -d' ' -f 2,6- | grep -a -e "$1"
    fi
}

hview() {
    if [ $# -eq 0 ]
    then 
        vim $LOGFILE
    else
        vim +$1 $LOGFILE
    fi
}
