# ----------------------------------------------------------------------------
# ------------------------------ SOURCES -------------------------------------
source /home/bryan/Dropbox/dotfiles/home/extras/oh-my-zsh

# Disable aliases
arr=("ll" "gco")
for i in "${arr[@]}"; do
    unalias "$i" &> /dev/null
done

# Disable builtins
disable r

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
alias -s xbm="imv -d"
alias -s png="imv -d"
alias -s pcx="imv -d"
alias -s jpg="imv -d"
alias -s jpeg="imv -d"
alias -s gif="imv -d"
# Miscellaneous
alias -s git="git clone"
alias -s html="google-chrome-stable"
alias -s avi="vlc"
alias -s txt="vim"

# --- Global Aliases
alias -g @@="&> /dev/null & disown"
alias -g ::="| grep -i -e"
alias -g %="| less"
alias -g X="clear &&"

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
_git 2> /dev/null  # hack to make git branch completion work
_pacman 2> /dev/null  # hack to make pacman completion work
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

if [[ -d $PWD/.git ]] && [[ -d ~/.virtualenvs/$(basename $PWD) ]]; then
    workon $(basename $PWD) &> /dev/null
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

if [[ -f $PWD/.lzshrc ]]; then
    printf "\n*** ALERT: A Local zshrc File has been Sourced ***\n\n"
    source ".lzshrc"
fi

if (( $+commands[tag] )); then
  export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
  tag() { command tag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
  alias ag=tag  # replace with rg for ripgrep
fi

# added by travis gem
[ -f /home/bryan/.travis/travis.sh ] && source /home/bryan/.travis/travis.sh

[ -f /home/bryan/.config/localalias/localalias.zsh ] && source /home/bryan/.config/localalias/localalias.zsh
