alias tm="tmux -2"
alias ta="tmux -2 attach"

alias sql="rlwrap sqlite3"

export TERM="screen-256color"
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $POWERLINE_DIRECTORY/powerline/bindings/bash/powerline.sh

# Enables vman command from the terminal
vman() {
  vim -c "SuperMan $*"

  if [ "$?" != "0" ]; then
    echo "No manual entry for $*"
  fi
}

# I set this so the crontab would use vim for editing
export EDITOR=$(which vim)

# Eternal bash history.
# ---------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=1000000000
export HISTSIZE=1000000000
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# 
# This also ensures that commands placed in a tmux session are reserved
# in history. 
# 
# http://superuser.com/questions/20900/bash-history-loss
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
