#################################
#  Shell Aliases and Functions  #
#################################

source /home/bryan/Dropbox/dotfiles/extras/gentoo.sh
source /home/bryan/Dropbox/dotfiles/extras/GTD.sh

alias Java='java -classpath .:../bin'
alias Javac='javac -d ../bin'
alias Jdb='jdb -classpath "../bin"'
alias ag='ag --hidden'
ainit() { printf "#!/bin/awk -f\n\n" > "$1.awk" && chmod +x "$1.awk" && clinks && vim + "$1.awk"; }
alias alg='alias | grep -e'
auto() { nohup autodemo "$@" &> /dev/null & disown && clear; }
binit() { printf "#!/bin/bash\n\n" > "$1" && chmod +x "$1" && clinks && vim + "$1"; }
alias cal='cal -n 3 | less'
alias ccat='pygmentize -g'
alias chx='sudo chmod +x'
alias cower='cower -c'
alias cp="cp -i"
alias cppinit='cinit ++'
cprof() { python -m cProfile -s "$@" | less; }
alias delshots='confirm "find /home/bryan/Dropbox/var/aphrodite-motion -name \"*$(date +%Y%m%d)*\" -delete"'
alias dfs='dropbox-cli filestatus'
alias dst='dropbox-cli status'
alias dstart='dropbox-cli start'
alias dstop='dropbox-cli stop'
alias flaggie='sudo -i flaggie'
alias du='ncdu --color dark'
alias gcignore='git add .gitignore && git commit -m "Update: .gitignore file"'
alias ggrep='git rev-list --all | xargs git grep -n --break --heading'
alias gho='ghi open'
alias ghooks='rm -rf .git/hooks && git init' 
alias gnlog="git log --oneline --color=always | nl -s ':  ' -v 0 | less"
alias gpf='git push -f'
alias grc='git rebase --continue'
gpup() { git add . && git commit -m "Update $(grepo)" && git push; }
gri() { git rebase -i HEAD~$1; }
alias ipython='TERM=linux ipython'
alias lay='sudo layman'
alias lim='vim -c "normal \`0" -c "bdelete 1"'
alias loc='locate -r'
alias lpass-login='lpass login bryanbugyi34@gmail.com'
alias matlab='matlab -nojvm -nodisplay -nosplash'
alias mirror='xrandr --output DVI-I-1-1 --auto --same-as LVDS1'
mkcd() { mkdir -p $1 && cd $1; }
alias mkdir='mkdir'
alias mkpkg='makepkg -si'
alias mv="mv -i"
alias nomirror='xrandr --output DVI-I-1-1 --auto --right-of LVDS1'
onething() { vim -c "/$(date --date="yesterday" +%m\\/%d\\/%Y)" ~/Dropbox/notes/Onething/"$1".txt; }
alias pipget='pip install --user'
alias pdb='ipdb'
alias psg='ps -aux | grep -E'
pvar() { set | grep -i -e "^$1"; }
cho() { sudo chown -R "$2":"$2" "$1"; }
alias reboot='sudo /sbin/reboot'
alias rg='ranger'
rip() { nohup torrent -d "$@" &> /dev/null & disown; }
alias rm="safe-rm"
alias sch='vim ~/Dropbox/notes/Rutgers/course_schedule.txt'
alias sc='systemctl'
alias scu='systemctl --user'
alias sftp-rutgers='sftp bmb181@less.cs.rutgers.edu'
alias shutdown='sudo poweroff'
alias snapshots='find /home/bryan/Dropbox/var/aphrodite-motion -name "*$(date +%Y%m%d)*" | sort | xargs imv && delshots'
ss() { tmux send-keys "sleep 1.5 && !-2" "Enter"; }
alias ssh-aphrodite='ssh -p 34588 bryan@aphrodite'
alias ssh-artemis="ssh root@67.207.92.152"
alias ssh-athena="ssh -p $ATHENAS_SSH_PORT bryan@$ATHENAS_DDNS_HOSTNAME"
alias ssh-rutgers='ssh bmb181@less.cs.rutgers.edu'
alias su='su - -p'
alias sudo='sudo '  # makes aliases visible to sudo
alias sudoers='sudo vim /etc/sudoers'
alias time='/usr/bin/time'
tmd() { tmux display-message -p "#{$1}"; }
alias tree='clear && tree -I "venv*|__pycache__*|coverage*"'
tsm() { transmission-remote -l; }
tsm-add() { transmission-remote -a $1; }
tsm-boost() { transmission-remote -t$1 -Bh -phall -pr250; }
tsm-purge() { transmission-remote -t$1 -rad; }
tsm-rm() { transmission-remote -t$1 -r; }
tsm-start() { transmission-daemon; }
tsm-stop() { killall -9 transmission-daemon; }
tsm-watch() { watch -n 1 transmission-remote -l; }
tws() { timew shorten @1 "$1"mins; }
alias updatedb='sudo updatedb'
vab() { vim $(find /home/bryan/Dropbox/scripts/bin/cron.jobs -type f | tr '\n' ' '); }
alias vdb='vim /home/bryan/Dropbox/scripts/bin/cron.jobs/cron.daily/*'
alias vwb='vim /home/bryan/Dropbox/scripts/bin/cron.jobs/cron.weekly/*'
alias vmb='vim /home/bryan/Dropbox/scripts/bin/cron.jobs/cron.monthly/*'
alias vcplug="vim -c ':PluginClean'"
alias vihor='vim ~/Dropbox/notes/Horizons_of_Focus/*'
alias vip="vim -c 'execute \"normal \\<c-p>\" '"
alias vplug="vim -c ':PluginInstall'"
vuse() { vim /etc/portage/package.use/"$1"; }
alias w='which'
alias wm='wmctrl -lx'
alias watdst='watch -n 5 dropbox-cli status'
