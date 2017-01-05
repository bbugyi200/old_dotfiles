# ----------------------------------------------------------------------------
# I set this so the crontab would use vim for editing
export EDITOR=$(which vim)

# ---------------------------- ALIASES ---------------------------------------
alias tm="tmux -2"
alias ta="tmux -2 attach"

alias sql="rlwrap sqlite3"

alias hgrep="cat ~/Dropbox/logs/* | grep "

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
export PROMPT_COMMAND+='if [ "$(id -u)" -ne 0 ]; then echo "$(hostname) | $(date "+%Y-%m-%d.%H:%M:%S") | $(pwd) | $(history 1 | cut -c 8-)" >> ~/Dropbox/logs/bash-history.log; fi'
