###############
#  Oh-My-Zsh  #
###############
ZSH=/home/bryan/.oh-my-zsh/
ZSH_THEME="mytheme"
DEFAULT_USER="bryan"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"

# 'sudo' plugin MUST remain near the end or (for some reason) it won't work
plugins=(git lpass vi-mode z zsh-autosuggestions zsh-syntax-highlighting sudo)

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

ZSH_DISABLE_COMPFIX="true"  # disable warning messages whenever I use 'su' to login as root

source $ZSH/oh-my-zsh.sh

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

################################
#  Disable Aliases / Builtins  #
################################
# Disable aliases
arr=("ll" "gcl" "gco", "gsta")
for i in "${arr[@]}"; do
    unalias "$i" &> /dev/null
done

# Disable builtins
disable r

####################
#  Autocompletion  #
####################
autoload -U +X compinit && compinit -u
autoload -U +X bashcompinit && bashcompinit
for filename in ~/.bash-completions/*; do
    source "$filename"
done

_git 2> /dev/null  # hack to make git branch completion work
compdef __git_branch_names gco
compdef _command_names wim vinfo
compdef _task tt ti tpi ts to ta tg tgw tgr tga tin tmi tget
compdef _tmuxinator tm
command -v emerge &> /dev/null && compdef sudo_del=emerge
command -v emerge &> /dev/null && compdef sudo_get=emerge
command -v rc-service &> /dev/null && compdef rcst=rc-service
compdef vman=man


#####################
#  Source Commands  #
#####################
function source_if_exists() {
    [[ -f "$1" ]] && source "$1"
}

source_if_exists /usr/local/lib/aliases.sh
source_if_exists /usr/local/lib/tmuxinator.zsh
source_if_exists /home/bryan/.local/share/funky/funky.sh
source_if_exists /home/bryan/.zprofile
source_if_exists /home/bryan/.fzf.zsh

##############
#  Settings  #
##############
setopt null_glob  # disables errors when GLOB pattern does not match
setopt globdots

#################
#  ZSH Aliases  #
#################
so() { exec /bin/zsh; }

# ---------- Suffix Aliases ----------
if [[ "$(uname -a)" == *"Debian"* ]]; then
    alias -s gif="imvr -d"
    alias -s jpeg="imvr -d"
    alias -s jpg="imvr -d"
    alias -s pcx="imvr -d"
    alias -s png="imvr -d"
    alias -s xbm="imvr -d"
else
    alias -s gif="imv -d"
    alias -s jpeg="imv -d"
    alias -s jpg="imv -d"
    alias -s pcx="imv -d"
    alias -s png="imv -d"
    alias -s xbm="imv -d"
fi

alias -s avi="vlc"
alias -s csv="libreoffice"
alias -s djvu="zathura"
alias -s doc="libreoffice"
alias -s docx="libreoffice"
alias -s epub="zathura"
alias -s git="git clone"
alias -s html="google-chrome-stable"
alias -s odt="libreoffice"
alias -s pdf="zathura"
alias -s ppt="libreoffice"
alias -s pptx="libreoffice"
alias -s ps="zathura"
alias -s txt="vim"
alias -s wav="paplay"
alias -s xls="libreoffice"
alias -s xlsx="libreoffice"

# ---------- Global Aliases ----------
alias -g @@="&> /dev/null & disown"
alias -g ::="| grep -i -e"
alias -g :::="| grep -A 5 -B 5 -i -e"
alias -g :c="clear &&"
alias -g :l="| less"
alias -g :L="tmux send-keys '!-2 | less' Enter Enter"
alias -g :w="watch -n 1"

##############
#  Bindings  #
##############
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search

###################
#  Miscellaneous  #
###################
ulimit -c unlimited  # Enables Core Dumps

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

if [[ -d $PWD/.git ]] && [[ -d ~/.virtualenvs/$(basename $PWD) ]]; then
    workon $(basename $PWD) &> /dev/null
fi

if [[ -d venv ]]; then
    source venv/bin/activate
fi

# Starts ssh-agent automatically
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)" > /dev/null
fi

# Needed for Eternal Command History
preexec() { log_shell_history "$1"; }

if [[ -f $PWD/.lzshrc ]]; then
    printf "\n*** ALERT: A Local zshrc File has been Sourced ***\n\n"
    source ".lzshrc"
fi

if (( $+commands[tag] )); then
  export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
  tag() { command tag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
  alias ag=tag  # replace with rg for ripgrep
fi

function command_not_found_handler() {
    cmd="$1"; shift

    if [[ "${cmd}" == "+"* ]]; then
        funky_cmd="funky -a ${cmd:1}"
    elif [[ "${cmd}" == "-"* ]]; then
        funky_cmd="funky -r ${cmd:1}"
    elif [[ "${cmd}" == "@"* ]]; then
        funky_cmd="funky -R ${cmd:1} $1"
    else
        >&2 printf "%s: %s\n" "zsh: command not found" "${cmd}"
        exit 127
    fi

    tmux send-keys "${funky_cmd}" "Enter"
}

test 0  # so exit status is always 0 when starting shell
