# ----------------------------------------------------------------------------
# ------------------------------ SOURCES -------------------------------------

source ~/Dropbox/dotfiles/extras/localalias
source ~/Dropbox/dotfiles/extras/globrc


# ------------------------------- LocalAlias ----------------------------------
# orig_command_not_found ---> command_not_found_handle
# http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
eval "$(echo "orig_command_not_found()"; declare -f command_not_found_handle | tail -n +2)"
command_not_found_handle() {
    GREP=$(grep -s "^$1$SEP" "./.localaliases")
    if LocalAlias $1 "$GREP" "${@:2}"; then
        echo
        orig_command_not_found "$1"
    fi
}

# ------------------------------- POWERLINE -----------------------------------
# Needed for powerline to work
export TERM="screen-256color"

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $POWERLINE_DIRECTORY/powerline/bindings/bash/powerline.sh
