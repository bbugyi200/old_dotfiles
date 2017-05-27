# ----------------------------------------------------------------------------
# ------------------------------ SOURCES -------------------------------------
export PATH=$PATH:~/Dropbox/bin:~/Dropbox/config
source localAlias
source globrc

# ----------------------------------- ALIASES --------------------------------
alias -s pdf="okular"
alias -s docx="/usr/lib/libreoffice/program/soffice.bin --writer"
alias -s ppt="/usr/lib/libreoffice/program/soffice.bin --impress"
alias -s png="eog"
alias -s jpg="eog"
alias -s jpeg="eog"
alias -s git="git clone"

alias -g @@="&> /dev/null & disown"
alias -g X="&& exit"

# -------------------------------- FUNCTIONS ---------------------------------

precmd() { eval "$PROMPT_COMMAND" }

# orig_command_not_found ---> command_not_found_handle
# http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
command_not_found_handler() {
    GREP=$(grep -s "^$1$SEP" "./.localaliases")
    if LocalAlias $1 "$GREP" "${@:2}"; then
          :
    fi
}
