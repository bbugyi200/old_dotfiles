# ----------------------------------------------------------------------------
# ------------------------------ SOURCES -------------------------------------
source /home/bryan/Dropbox/dotfiles/extras/oh-my-zsh
source /home/bryan/Dropbox/dotfiles/.globrc

# -------------------------- OH-MY-ZSH ---------------------------------------
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

# ----------------------------------- ALIASES --------------------------------
alias -s pdf="zathura"
alias -s epub="zathura"
alias -s djvu="zathura"
alias -s docx="/usr/lib/libreoffice/program/soffice.bin --writer"
alias -s odt="/usr/lib/libreoffice/program/soffice.bin --writer"
alias -s ppt="/usr/lib/libreoffice/program/soffice.bin --impress"
alias -s png="imv"
alias -s pcx="imv"
alias -s jpg="imv"
alias -s jpeg="imv"
alias -s git="git clone"

alias -g @@="&> /dev/null & disown"
alias -g X="&& exit"
alias -g ::="| grep"

# -------------------------------- BINDINGS ----------------------------------
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey -s "^[-" "popd && clear\n"

# -------------------------------- FUNCTIONS ---------------------------------

# Starts ssh-agent automatically
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)" > /dev/null
fi

precmd() { eval "$PROMPT_COMMAND" }
# # orig_command_not_found ---> command_not_found_handle
# # http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
command_not_found_handler() {
    ALIAS=$1; shift
    LocalAlias --evaluate=$ALIAS -- "$@"
}
