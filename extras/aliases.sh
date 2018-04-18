source /home/bryan/Dropbox/dotfiles/extras/GTD.sh

# ---------- MISCELLANEOUS ----------
alias c='clear'
alias delshots='confirm "find /home/bryan/Dropbox/logs/aphrodite-motion -name \"*$(date +%Y%m%d)*\" -delete"'
alias la="LocalAlias"
lls () { clear && ls -a && echo; }
lcd () { cd "$1" && lls; }
alias mirror='xrandr --output DVI-I-1-1 --auto --same-as LVDS1'
# Create and CD to New Directory
mkcd() { mkdir -p $1 && cd $1; }
alias mutt-gmail="neomutt -e 'source ~/.mutt/hooks/bryanbugyi34.gmail'"
alias mutt-root='sudo neomutt -f /var/spool/mail/root'
alias mutt="neomutt"
alias nomirror='xrandr --output DVI-I-1-1 --auto --right-of LVDS1'
alias psgrep='ps -aux | grep -E'
# Prints the value of a variable
pvar () { set | grep -i -e "^$1"; }
alias snapshots='find /home/bryan/Dropbox/logs/aphrodite-motion -name "*$(date +%Y%m%d)*" | sort | xargs imv && delshots'
# Sleep, then run the last command
ss () { tmux send-keys "sleep 1.5 && !-2" "Enter"; }
w() { which $1 || (printf "\n----- Searching Local Aliases/Functions... -----\n" && la $1); }

# ---------- Vim ----------
alias lim='vim -c "normal \`0" -c "bdelete 1"'
alias onething='vim -c "/$(date --date="yesterday" +%m\\/%d\\/%Y)" ~/Dropbox/notes/Onething/{body.txt,mind.txt,heart.txt}'
alias sch='vim ~/Dropbox/notes/Rutgers/course_schedule.txt'
alias vihor='vim ~/Dropbox/notes/Horizons_of_Focus/*'
alias vip="vim -c 'execute \"normal \\<c-p>\" '"
pim() { vim -c 'CtrlPMRU' -c "normal $1" -c 'execute "normal \<cr>"'; }

# ---------- SSH / FTP ----------
alias sftp-rutgers='sftp bmb181@less.cs.rutgers.edu'
alias ssh-aphrodite='ssh -p 34588 bryan@aphrodite'
alias ssh-athena="ssh -p $ATHENAS_SSH_PORT bryan@$ATHENAS_DDNS_HOSTNAME"
alias ssh-artemis="ssh root@67.207.92.152"
alias ssh-rutgers='ssh bmb181@less.cs.rutgers.edu'

# ---------- Java ----------
alias Java='java -classpath .:../bin'
alias Javac='javac -d ../bin'
alias Jdb='jdb -classpath "../bin"'

# ---------- Git ----------
alias gcignore='git add .gitignore && git commit -m "Update: .gitignore file"'
alias ggrep='git rev-list --all | xargs git grep -n --break --heading'
alias ghooks='rm -rf .git/hooks && git init' 
alias gnum="git log --oneline --color=always | nl -s ':  ' | less"
alias gpf='git push -f'
alias gho='ghi open'
alias algrep='alias | grep -e'
alias grc='git rebase --continue'
gri() { git rebase -i HEAD~$1; }

# ---------- Overriden Commands ----------
alias ag='ag --hidden'
alias cal='cal -n 3 | less'
alias ccat='pygmentize -g'
alias cower='cower -c'
alias matlab='matlab -nojvm -nodisplay -nosplash'
alias sudo='sudo '  # makes aliases visible to sudo
alias time='/usr/bin/time'
alias tree='tree -I "venv*|__pycache__*|coverage*"'
loc() { /usr/bin/locate -r ".*$@.*"; }
# Remove/Overwrite Files Safely
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"

# ---------- transmission-remote ----------
tsm() { transmission-remote -l; }
tsm-start() { transmission-daemon; }
tsm-stop() { killall -9 transmission-daemon; }
tsm-watch() { watch -n 1 transmission-remote -l; }
tsm-add() { transmission-remote -a $1; }
tsm-boost() { transmission-remote -t$1 -Bh -phall -pr250; }
tsm-rm() { transmission-remote -t$1 -r; }
tsm-purge() { transmission-remote -t$1 -rad; }
rip() { nohup torrent "$@" &> /dev/null & disown; }

# ---------- LangDoc ----------
alias PyDoc='LangDoc -e py -m'
alias JavaDoc='LangDoc -e java -m'
alias CDoc='LangDoc -e c -m'
alias Doc='LangDoc -e sh -m'

# ---------- Package Management ----------
alias update='confirm "sudo pacman -Syu"; confirm "mkdir /tmp/builds; cd /tmp/builds && cower -vud && buildall"'
alias del='sudo pacman -Rs'
alias mkpkg='makepkg -si'

# ---------- Python
alias pdb='python -m pdb'
cprof() { python -m cProfile -s "$@" | less; }
