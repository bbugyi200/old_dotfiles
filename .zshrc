########################################
#  Startup Completion Scripts / Setup  #
########################################
# Brew ZSH Completions
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

###############
#  Oh-My-Zsh  #
###############
ZSH="$HOME"/.oh-my-zsh/
ZSH_THEME="mytheme"
DEFAULT_USER="bryan"
DISABLE_AUTO_TITLE="true"

# 'sudo' plugin MUST remain near the end or (for some reason) it won't work
plugins=(desk docker docker-compose git git-extras emoji vi-mode z zsh-autosuggestions zsh-syntax-highlighting sudo)

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
bad_aliases=("diff" "ll" "gcl" "gca" "gcm" "gco" "gd" "gg" "glg" "gpu" "gra" "gsta" "gwip")
for i in "${bad_aliases[@]}"; do
    unalias "$i" &> /dev/null
done

# Disable builtins
disable r

#####################
#  Source Commands  #
#####################
function source_if_exists() { [[ -f "$1" ]] && source "$1"; }

source_if_exists "${HOME}"/.config/aliases.sh
source_if_exists /usr/local/lib/tmuxinator.zsh
source_if_exists "$HOME"/.local/share/funky/funky.sh
source_if_exists "$HOME"/.zprofile
source_if_exists "$HOME"/.fzf.zsh

##############
#  Settings  #
##############
unsetopt BEEP  # Disable automatic terminal bells (e.g. tab-completion)
setopt globdots
setopt null_glob  # disables errors when GLOB pattern does not match

#################
#  ZSH Aliases  #
#################
so() { exec /bin/zsh; }

# ---------- Suffix Aliases ----------
if [[ "$(uname -a)" == *"Debian"* ]]; then
    alias -s gif="imv-x11 -d"
    alias -s jpeg="imv-x11 -d"
    alias -s jpg="imv-x11 -d"
    alias -s pcx="imv-x11 -d"
    alias -s png="imv-x11 -d"
    alias -s xbm="imv-x11 -d"
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
alias -s html="qutebrowser"
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
alias -g @!="&> /dev/null"
alias -g @@="&> /dev/null & disown"
alias -g :bb:="C02DR3Z2MD6R:8888"
alias -g :g="| grep -i -e"
alias -g :G="| grep -A 5 -B 5 -i -e"
alias -g :c="clear &&"
alias -g :l="| less"
alias -g :L="tmux send-keys '!-2 | less' Enter Enter"
alias -g :p="| tr ':' '\\n'"
alias -g :w="watch -n 1"

##############
#  Bindings  #
##############
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey -M vicmd v edit-command-line

###################
#  Miscellaneous  #
###################

# ulimit -c unlimited  # Enables Core Dumps
stty -ixon  # So as not to be disturbed by Ctrl-S ctrl-Q in terminals.

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
preexec() { shw.sh &> /dev/null "$1"; }

if [[ -f $PWD/.lzshrc ]]; then
    source ".lzshrc"
fi

function command_not_found_handler() {
    cmd="$1"; shift

    if [[ "${cmd}" == "++"* ]]; then
        funky_cmd="pushd $HOME > /dev/null && funky -a ${cmd:2} && popd > /dev/null"
    elif [[ "${cmd}" == "+"* ]]; then
        funky_cmd="funky -a ${cmd:1}"
    elif [[ "${cmd}" == "--"* ]]; then
        funky_cmd="pushd $HOME > /dev/null && funky -r ${cmd:2} && popd > /dev/null"
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

# Hook for desk activation
[ -n "$DESK_ENV" ] && source "$DESK_ENV" || true

# Check if command exists.
function cmd_exists() { command -v "$1" &>/dev/null; }

# pyenv
export PATH="/home/bryan/.pyenv/bin:$PATH"
if cmd_exists pyenv && [[ -z "${VIRTUAL_ENV}" ]]; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

cmd_exists starship && eval "$(starship init zsh)"

if cmd_exists virtualenvwrapper_lazy.sh; then
    export WORKON_HOME="${HOME}"/.virtualenvwrapper-venvs
    export PROJECT_HOME="${HOME}"/projects

    if cmd_exists pyenv; then
        pyenv virtualenvwrapper_lazy
    else
        source virtualenvwrapper_lazy.sh
    fi
fi

####################
#  Autocompletion  #
####################
autoload -U +X compinit && compinit -u
autoload -U +X bashcompinit && bashcompinit
if [[ -d ~/.bash-completions ]]; then
    for filename in ~/.bash-completions/*; do
        source "$filename"
    done
fi

# pipx
eval "$(register-python-argcomplete pipx)"

_git 2> /dev/null  # hack to make git branch completion work
compdef _command_names wim vinfo
compdef _git-checkout gco gnext
compdef _git-diff gd
compdef _git-log glg
compdef _task tt ti tpi ts to ta tg tgw tgr tga tin tmi tget
compdef _tmuxinator tm
compdef vman=man

command -v emerge &> /dev/null && compdef sudo_del=emerge
command -v emerge &> /dev/null && compdef sudo_get=emerge
command -v rc-service &> /dev/null && compdef rcst=rc-service

# pip completion
eval "$(python -m pip completion --zsh 2>/dev/null)"
test 0  # so exit status is always 0 when starting shell
