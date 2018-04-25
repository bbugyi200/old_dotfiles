# ----------------------------------------------------------------------------
# ------------------------------ SOURCES -------------------------------------
source /home/bryan/Dropbox/dotfiles/home/extras/oh-my-zsh

unalias gco  # Defined in Oh-My-Zsh's Git plugin
unalias d; unalias l; unalias ll; disable r
unalias 1; unalias 2; unalias 3; unalias 4; unalias 5; unalias 6; unalias 7; unalias 8; unalias 9

source /home/bryan/Dropbox/dotfiles/home/extras/tmuxinator.zsh
source /home/bryan/Dropbox/dotfiles/home/extras/aliases.sh
source /home/bryan/Dropbox/dotfiles/home/extras/eternal_history.sh

# -------------------------- OH-MY-ZSH ---------------------------------------
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

# ------------------------------ ZSH ALIASES ---------------------------------
alias so='unalias -a && source ~/.zshrc'

# --- Suffix Aliases
# Zathura
alias -s pdf="zathura"
alias -s epub="zathura"
alias -s djvu="zathura"
alias -s ps="zathura"
# LibreOffice
alias -s doc="/usr/lib/libreoffice/program/soffice.bin --writer"
alias -s docx="/usr/lib/libreoffice/program/soffice.bin --writer"
alias -s odt="/usr/lib/libreoffice/program/soffice.bin --writer"
alias -s ppt="/usr/lib/libreoffice/program/soffice.bin --impress"
alias -s pptx="/usr/lib/libreoffice/program/soffice.bin --impress"
# Imv
alias -s xbm="imv"
alias -s png="imv"
alias -s pcx="imv"
alias -s jpg="imv"
alias -s jpeg="imv"
# Miscellaneous
alias -s git="git clone"
alias -s html="google-chrome-stable"
alias -s avi="vlc"
alias -s txt="vim"

# --- Global Aliases
alias -g @@="&> /dev/null & disown"
alias -g ::="| grep -e"
alias -g %="| less"

# -------------------------------- BINDINGS ----------------------------------
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search

# ------------------------------ EXPORTS ---------------------------------------
# I set this so the crontab would use vim for editing
export EDITOR=$(which vim)

export PROMPT_COMMAND="log_bash_history" 
# Removed to fix double history log # ; $PROMPT_COMMAND"

# Fixes Mutt Background Issue (stays transparent) in TMUX
export TERM="screen-256color"

# ------------------------------ AUTOCOMPLETION ------------------------------
_git 2>/dev/null  # hack to make git branch completion work
compdef __git_branch_names gco
compdef _command_names wim
compdef _pacman_completions_all_packages get
compdef _task tt ti tpi ts to ta tg tgw tgr tga tin tmi tget

autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
for filename in ~/.bash-completions/*; do
    source "$filename"
done

# -------------------------------- MISCELLANEOUS -----------------------------
#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

if [[ -d $PWD/venv ]]; then
	source "venv/bin/activate"
fi

# Starts ssh-agent automatically
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)" > /dev/null
fi

# Needed for Eternal Command History
precmd() { eval "$PROMPT_COMMAND" }

command_not_found_handler() {
    WORD=$1; shift
    LocalAlias -x $WORD -- "$@"
}

if [[ -f $PWD/.lzshrc ]]; then
    printf "\n*** ALERT: A Local zshrc File has been Sourced ***\n\n"
    source ".lzshrc"
fi
