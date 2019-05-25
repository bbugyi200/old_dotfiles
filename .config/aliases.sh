#!/bin/bash

#################################
#  Shell Aliases and Functions  #
#################################

# shellcheck disable=SC1010
# shellcheck disable=SC2009
# shellcheck disable=SC2079
# shellcheck disable=SC2142
# shellcheck disable=SC2230


sys_info="$(uname -a)"
if [[ "${sys_info}" == *"gentoo"* ]]; then
    source "$HOME/.config/gentoo.sh"
elif [[ "${sys_info}" == *"Debian"* ]]; then
    source "$HOME/.config/debian.sh"
elif [[ "${sys_info}" == *"Darwin"* ]]; then
    source "$HOME/.config/macos.sh"
fi


# ---------- cookie Aliases / Functions ----------
# def marker: COOKIE
alias ainit='cookie template.awk -D awk -x'
alias binit='cookie minimal.sh -x'
alias Binit='cookie full.sh -x'
cinit() { PROJECT="$1" cookie -f -t c.mk Makefile && cookie -f -t main.c src/main.c && cookie -f -t gtest.mk tests/Makefile && cookie -f -t main.gtest tests/main.cc; }
alias co='cookie'
hw() { mkdir -p HW"$1"/img &> /dev/null && ASSIGNMENT_NUMBER="$1" cookie hw.tex -f "${@:2}" HW"$1"/hw"$1".tex; }
alias minit='cookie c.make -f Makefile'
alias mtinit='cookie gtest.make -f Makefile'
alias pyinit='cookie template.py -x'
robinit() { DATE="$(date +%Y%m%d)" cookie robot.yaml -f "$HOME"/.local/share/red_robot/pending/"$1"; }
pytinit() { SCRIPT="$1" cookie pytest_script.py -f test_"$1".py; }
alias texinit='cookie template.tex -f'
zinit() { MY_SCRIPTNAME="$1" sudo -E cookie _zsh_completion -f /usr/share/zsh/site-functions/_"$1"; }

# ---------- GTD Aliases / Functions ----------
# def marker: GTD
ka() { "$HOME"/.local/bin/ka "$@" && krestart_alarms; }
kc() { clear && khal calendar --notstarted --format '{start-time} {title} [{location}]' now && echo; }
ke() { khal edit "$@" && krestart_alarms; }
ki() { ikhal "$@" && krestart_alarms; }
alias knh='khal new -a home'
krestart_alarms() { setsid calalrms -d &> /dev/null; }
alias ta='task add'
tas() { tmux send-keys "ta project:Study.$*" "Enter"; };
tc () { clear && task next rc.verbose=blank,label rc.defaultwidth:$COLUMNS +READY limit:page; }
tcd() { task done "$1" && tc; }
alias tcn='task context none && task_refresh -F rename,config'
alias tcomp='task limit:10 \( status:completed or status:deleted \) rc.report.all.sort:end- all'
tcs () { task rc.context="$1" "${@:2}"; }
tcsn () { tcs none "$@"; }
tcx () { task context "$@" && task_refresh -F rename,config; }
alias td='task done'
alias tdel='task delete'
# shellcheck disable=SC1010
tdi () { task "$(tnext_inbox_id)" done; }
alias tdue='tga +OVERDUE'
tga () { eval "tcsn rc.verbose=blank,label rc.defaultwidth:$COLUMNS $* -COMPLETED -DELETED all"; }
tget () { task _get "$@"; }
tgp () { eval "tga project:$*"; }
tgps () { eval "tgp Study.$*"; }
tgs() { tga project:Study."$*"; }
tgw () { eval "tcsn $* rc.verbose=blank,label waiting"; }
ti () { task rc._forcecolor:on "$@" info | less -F; }
tin () { task rc.context=none +inbox -DELETED -COMPLETED all; }
tl () { task "$1" | less -F; }
alias tlat='task rc._forcecolor:on +LATEST info | less -F'
tnall () { tcsn "next +READY"; }
tnl () { task next +READY limit:none; }  # no limit
tpa () { tga project:"$(tproject)"; }
trev () { task rc.context:review rc.verbose:nothing rc.defaultwidth:$COLUMNS limit:none \( +PENDING or +WAITING \) | less -F; }
alias tstudy='vim ~/.vimwiki/TaskWarrior.wiki'
tsub () { task "$1" modify "/$2/$3/g"; }
alias tw='timew'
alias wdel='if ! watson_is_on; then watson remove -f $(watson frames | tail -n 1); else return 1; fi'
alias wedit='watson edit'
alias wlog='watson log'
alias wstat='watson status'

# ---------- Mutt Aliases / Functions ----------
# def marker: MUTT
alias bmutt='TERM=rxvt-unicode-256color neomutt -f /var/spool/mail/bryan'
alias mutt="TERM=rxvt-unicode-256color neomutt"
alias rmutt="TERM=rxvt-unicode-256color neomutt -e 'source ~/.mutt/hooks/bmb181@scarletmail.rutgers'"
alias sudo-mutt='TERM=rxvt-unicode-256color sudo neomutt -f /var/spool/mail/root'
alias vmutt='vim $HOME/.mutt/muttrc'

# ---------- Vim Wrapper Aliases / Functions ----------
# def marker: VIM
def() { zim "def" "$@" "$HOME/Dropbox/home/.zshrc" "$HOME/Dropbox/home/.config/aliases.sh" "$HOME/Dropbox/home/.config/debian.sh" "$HOME/Dropbox/home/.config/gentoo.sh"; }
lim() { vim -c "normal '0" -c 'bd1'; }
mim() { zim "mim" "$@"; }
alias pim="F=\"\$(rg --files | fzf)\"; [[ -n \"\${F}\" ]] && vim \"\${F}\""
tam() { N="$(history -n | tail -n 100 | tac | nl | fzf --tiebreak=index | awk '{print $1}')"; if [[ -n "${N}" ]]; then tim "${N}" "$@"; fi; }
tim() { f=$(fc -e - -"${1:-1}" 2> /dev/null | fzf -q "$2"); if [[ -n "${f}" ]]; then vim "${f}"; fi; }
alias v='vim'
wim() { zim "wim" "$@"; }
zim() { "$HOME"/.local/bin/zim "$@" || { EC="$?"; if [[ "${EC}" -eq 3 ]]; then so; else return "${EC}"; fi; }; }

# ---------- File Copy / Cut / Paste ----------
p() { echo "The following files have been pasted into ${PWD}/${1}:" && ls -A /tmp/copy && /bin/mv -i /tmp/copy/* "${PWD}"/"${1}"; }
x() { mkdir /tmp/copy &> /dev/null; /bin/mv "$@" /tmp/copy/; }
y() { mkdir /tmp/copy &> /dev/null; /bin/cp -r "$@" /tmp/copy/; }

# ---------- Salary ----------
hourly_salary() { printf "%f\n" $(($(weekly_salary "$1") / 40.0)); }
hsal() { sal "$(($1 * 40.0 * 52.0 / 1000.0))" "${@:2}"; }
monthly_salary() { printf "%f\n" $(($(yearly_salary "$1") / 12.0)); }
NET_P=$((1.0 - TAX_P))
sal() { clear && salary "$@" && echo; }
salary() { printf "======= BEFORE TAXES =======\n" && _salary "$1" 0 && printf "\n===== AFTER TAXES (%0.0f%%) =====\n" "${2:-$((TAX_P * 100))}" && _salary "$@"; }
_salary() { { [[ -n "$2" ]] && NET_P=$((1.0 - ($2 / 100.0))); }; printf "Hourly:       $%0.2f\nWeekly:       $%0.2f\nBiweekly:     $%0.2f\nSemi-monthly: $%0.2f\nMonthly:      $%0.2f\nYearly:       $%0.2f\n" "$(hourly_salary "$1")" "$(weekly_salary "$1")" "$((2 * $(weekly_salary "$1")))" "$((0.5 * $(monthly_salary "$1")))" "$(monthly_salary "$1")" "$(yearly_salary "$1")"; NET_P=$((1.0 - TAX_P)); }
TAX_P=0.33;  # Default tax percentage used for salary calculation.
weekly_salary() { printf "%f\n" $(($(yearly_salary "$1") / 52.0)); }
yearly_salary() { printf "%f\n" $(($1 * 1000.0 * NET_P)); }

# ---------- Miscellaneous Aliases / Functions ----------
# def marker: DEFAULT
alias activate='source venv/bin/activate'
addgroup() { sudo usermod -aG "$1" bryan; }
alias ag='ag --hidden'
alg() { { alias | grep --color=always -e "$@" && echo; rg -s -C 5 -p "$@" ~/Dropbox/bin; } | less -F; }
alias anki='xspawn anki'
auto() { nohup autodemo "$@" &> /dev/null & disown && clear; }
bgdb() { gdb "$1" -ex "b $2" -ex "run"; }
alias c='cookie'
alias cal='cal -n 3 | less -F'
alias ccat='pygmentize -g'
ccd() { cd "$HOME/.cookiecutters/$1/{{ cookiecutter.project|lower }}" &> /dev/null || return 1; }
alias cdef='def -m COOKIE'
alias cdow='cd "$(dow_dir $PWD)"'
cho() { sudo chown -R "$2":"$2" "$1"; }
alias chrome='xspawn -w web google-chrome-stable'
alias chx='sudo chmod +x'
alias cower='cower -c'
alias cp="cp -i"
alias cplug='vim +PluginClean +qall'
alias cppinit='cinit ++'
cprof() { python -m cProfile -s "$@" | less -F; }
alias dayplan='cd $HOME/Dropbox/var/notes && vim dayplan.txt'
dc() { sudo -E deluge-console "${@}"; }
dci() { dc info --sort=time_added | awk -F ':' "{if(\$1==\"$1\")print \$0}"; }
alias ddef='def -m DEBIAN'
alias ddwrt-logs='sim /var/log/syslog-ddwrt'
alias deff='def -f'
alias del_swps='find . -name "*.swp" -delete -print'
alias delshots='confirm "find $HOME/Dropbox/var/aphrodite-motion -name \"*$(date +%Y%m%d)*\" -delete"'
alias df='df -h'
diff() { colordiff -wy -W "$(tput cols)" "$@" | less -F -R; }
alias dfs='dropbox-cli filestatus'
alias dst='dropbox-cli status'
alias dstart='dropbox-cli start'
alias dstop='dropbox-cli stop'
alias du='ncdu --color dark'
alias dunst='killall dunst &> /dev/null; dunst'
alias edsl='printf "$(hostname):%d,%d\n%s,%d" $(emanage -D local -u) $(emanage -D local -c) $(emanage -D remote -u) $(emanage -D remote -c | awk -F: "{print \$2}")'
alias epuse='sudo -E epuse'
alias flaggie='sudo -i flaggie'
fn_() { if [[ "$1" == *"*"* ]]; then find . -iname "$@"; else find . -iname "*$**"; fi; }
alias fn='noglob fn_'
fp_() { if [[ "$1" == *"*"* ]]; then find . -ipath "$@"; else find . -ipath "*$**"; fi; }
alias fp='noglob fp_'
forever() { while true; do eval "$*"; done; }
alias ga='git add -v'
alias gaa='git add -v --all'
alias gau='git add -v --update'
gcl() { cd "$("$HOME"/.local/bin/gcl "$@")" || return 1; }
alias gclp='cd ~/projects && gcl'
alias gclt='cd /tmp && gcl'
alias gcignore='git add .gitignore && git commit -m "Update: .gitignore file"'
gd() { git diff HEAD~"${1:-0}"; }
alias Gdef='def -m GTD'
alias gdef='def -m GENTOO'
alias gdo='git diff origin/master'
alias ggrep='git rev-list --all | xargs git grep -n --break --heading'
alias gho='ghi open'
alias ghooks='rm -rf .git/hooks && git init' 
alias ginit='while true; do; watch -d -n 1 cat .gdbinit; vim .gdbinit; done'
alias git='hub'
alias Glg='git log -p -G'
alias glg='git log --oneline --decorate --graph'
alias glg="git log --oneline --decorate --graph --color=always | nl -s ':  ' -v 0 | less -F"
alias gn='gnext'
alias gN='git checkout HEAD~1'
alias gpa='git commit -v -a --no-edit --amend && git push --force'
alias gpf='git push --force'
alias gprm='gpup "Docs: Update README"'
alias gpu='sudo radeontop -l 1 -d - | sed "1d" | head -n 1 | awk "{print \$3}" | sed "s/,//g"'
alias grc='git rebase --continue'
gri() { git rebase -i HEAD~"$1"; }
grun() { [[ "$(tail -n 1 "${PWD}"/.gdbinit)" == "r" ]] && sed -i '/^r$/d' "${PWD}"/.gdbinit || printf "r\n" >> "${PWD}"/.gdbinit; }
alias gsd='sudo get-shit-done'
header() { clear && eval "$@" && echo; }
info() { pinfo "$@" || { printf "\n===== APROPOS OUTPUT =====\n"; apropos "$@"; }; }
alias ipdb='ipdb3'
alias iplug='vim +PluginInstall +qall'
alias issh='ssh -p 34857 athena-arch.ddns.net'
K() { tmux switchc -n && tmux kill-session -t "$(tm-session-name)"; }
alias k='sudo kill'
alias k9='sudo kill -9'
alias kman='man -k'
Kman() { man -wK "$@" | awk -F'/' '{print $NF}' | sed 's/\.\(.*\)\.bz2$/ (\1)/g' | sort; }
alias lay='sudo layman'
alias Loc='sudo updatedb && loc'
alias loc='sudo locate --regex'
alias lpass-login='lpass login bryanbugyi34@gmail.com'
alias ls='ls --color=auto'
m-torrent() { echo "torrent -w /media/bryan/hercules/media/Entertainment/Movies $*" | at 0200; }
alias matlab='matlab -nojvm -nodisplay -nosplash'
alias mirror='xrandr --output DVI-I-1-1 --auto --same-as LVDS1'
mkcd() { mkdir -p "$1" && cd "$1" || return 1; }
alias mkdir='mkdir'
alias mkpkg='makepkg -si'
alias mpvlc='xspawn -w mpv mpvlc'
alias mv="mv -i"
alias myip='ip addr | grep -P -o "192.168.1.[0-9]+" | grep -v 192.168.1.255'
alias nomirror='xrandr --output DVI-I-1-1 --auto --right-of LVDS1'
alias nrg='pushd ~/Dropbox/var/notes && ranger && popd'
alias ok='xspawn okular'
onething() { vim -c "/$(date --date="yesterday" +%m\\/%d\\/%Y)" ~/Dropbox/var/notes/Onething/"$1".txt; }
alias ping2all='(ping2life 10.1.1.1; ping2life google.com) @@'
alias pipget='pip install --user'
alias pdb='pudb3'
pgr() { pgrep -f ".*$1.*"; }
alias plex='xspawn -w plex plexmediaplayer'
pname() { pass show | grep -i "$1" | awk '{print $2}'; }
alias pr1='ssh pr-dev-1'
alias psg='ps -ax | grep -v grep | grep'
alias pstrace="strace \$@ -p \$(ps -ax | fzf | awk '{print \$2}')"
pvar() { set | grep -i -e "^$1"; }
pycov() { coverage run "$1" && coverage html && qutebrowser htmlcov/index.html; }
alias reboot='sudo reboot'
rg() { "$(which -a rg | tail -n 1)" -p "$@" | less -F; }
ripmov() { nohup torrent -dv -w /media/bryan/hercules/media/Entertainment/Movies "$@" &> /dev/null & disown; }
riptv() { nohup torrent -dv -w /media/bryan/hercules/media/Entertainment/TV "$@" &> /dev/null & disown; }
alias rrg='cat "$RECENTLY_EDITED_FILES_LOG" | sudo xargs rg 2> /dev/null'
alias rag='cat $RECENTLY_EDITED_FILES_LOG | sudo xargs ag 2> /dev/null'
alias sch='vim ~/Dropbox/var/notes/Rutgers/course_schedule.txt'
alias sc='systemctl'
alias scu='systemctl --user'
alias sftp-rutgers='sftp bmb181@less.cs.rutgers.edu'
alias sim='sudo -E vim'
alias snapshots='find $HOME/Dropbox/var/aphrodite-motion -name "*$(date +%Y%m%d)*" | sort | xargs imv && delshots'
ss() { tmux send-keys "sleep 1.5 && !-2" "Enter"; }
alias ssh-aphrodite='ssh 192.168.1.193'
alias ssh-artemis="ssh bryan@67.207.92.152"
ssh-rutgers() { ssh bmb181@"${1:-less}".cs.rutgers.edu; }
alias su='su - -p'
alias sudo='sudo -E '  # makes aliases visible to sudo
alias sudoers='sudo -E vim /etc/sudoers'
alias sqlite3='rlwrap -a -N -c -i sqlite3'
alias tgdb="gdb -iex 'set pagination off' -ex 'tui enable' -ex 'set pagination on'"
alias time='/usr/bin/time'
tmd() { tmux display-message -p "#{$1}"; }
# shellcheck disable=SC2142
alias tm-layout="tmux lsw | grep '*' | awk '{gsub(/\\]/, \"\"); print \$7}'"
alias top='sudo htop'
alias tree='clear && tree -I "venv*|__pycache__*|coverage*"'
alias tsm='transmission-remote'
tsm-add() { transmission-remote -a "$1"; }
tsm-boost() { transmission-remote -t"$1" -Bh -phall -pr250; }
tsm-purge() { transmission-remote -t"$1" -rad; }
tsm-rm() { transmission-remote -t"$1" -r; }
tsm-start() { transmission-daemon; }
tsm-stop() { killall -9 transmission-daemon; }
tsm-watch() { watch -n 1 transmission-remote -l; }
tv-torrent() { echo "torrent -w /media/bryan/hercules/media/Entertainment/TV $*" | at 0200; }
u() { echo -e "\u$1"; }
alias undow='dow --undo'
alias updatedb='sudo updatedb'
# shellcheck disable=SC2046
vab() { vim $(find "$HOME"/Dropbox/bin/cron.jobs -type f | sort | tr '\n' ' '); }
alias valg='valgrind --leak-check=full --show-reachable=yes --track-origins=yes'
alias Vgi='vim ~/.gitignore_global'
alias vbox='xspawn sudo virtualbox'
alias vbt='vim ~/.local/share/torrent/*.txt'
alias vdb='vim $HOME/Dropbox/bin/cron/cron.daily/*'
alias vdiff='vimdiff -n'
venv() { vim "$HOME"/.zprofile "$HOME"/.profile "$HOME"/Dropbox/etc/environment "$(find "$HOME"/Dropbox/etc/profile.d -type f)" "$HOME"/.local/bin/etc-generator; }
alias vgdb-l='voltron view command "cmds set listsize $(tput lines) ; list *\$pc" --lexer c'
alias vgdb='vim ~/.gdbinit .gdbinit'
alias vgutils='vim /usr/bin/gutils.sh'
alias vi='vinfo'
alias vihor='vim ~/Dropbox/var/notes/Horizons_of_Focus/*'
alias vimilla='vim -u ~/.vanilla-vimrc'
vlog() { vim + /var/tmp/"$1"; }
alias vm='vman'
alias vmb='vim $HOME/Dropbox/bin/cron/cron.monthly/*'
alias vmkrules='make -p > /tmp/make-rules && vim /tmp/make-rules'
alias vpyutils='pushd ~/Dropbox/lib/python/gutils &> /dev/null && vv && popd &> /dev/null'
alias vr='vim ${RECENTLY_EDITED_FILES_LOG}'
alias vs='vshlog'
alias vscratch='vim ~/Dropbox/var/notes/scratch.txt'
alias vsd='vshlog -H all -D'
alias vstudy='vim $HOME/.vimwiki/TaskWarrior.wiki'
alias vsu='vshlog -u -D BOT EOT -H all -G'
alias vtorr='vim $HOME/.local/share/torrent/{tv.txt,movies.txt}'
alias vtv="vim \$HOME/.local/bin/tmux_view.sh \$HOME/.local/bin/tv_*"
alias vwb='vim $HOME/Dropbox/bin/cron/cron.weekly/*'
vrobot() { vim "$HOME"/.local/share/red_robot/pending/"$1"; }
vuse() { vim /etc/portage/package.use/"$1"; }
alias vq='vv_push ~/.config/qutebrowser'
vv_push() { tmux send-keys "clear && pushd '$1' &> /dev/null && vv && popd &> /dev/null && clear" "Enter"; }
alias vx='vv_push ~/.xmonad'
alias vxorg='sudo -E vim /etc/X11/xorg.conf.d/*'
alias w='which'
alias watdst='watch -n 5 dropbox-cli status'
alias wcut='watson stop && wedit && watson restart'
wdiff() { /usr/bin/wdiff -n -w "$(tput bold;tput setaf 1)" -x "$(tput sgr0)" -y "$(tput setaf 2)" -z "$(tput sgr0)" "$@" | less -R; }
alias wkill='wtoggle && wdel'
alias wm='wmctrl -lx'
alias wrep='watson report -w'
alias wsensors='watch -n 1 sensors -f'
alias wwat='watch -n 1 "{ wpoll; echo; watson log; }"'
alias xc='xclip -sel clipboard'
alias xdokey='xev -event keyboard'
alias xk='xdokey'
alias xmonad-keycodes='vim /usr/include/X11/keysymdef.h'
alias xs='xspawn'
ytd() { youtube-dl "$(xclip -sel clipboard -out)" --output "$1"; }
alias zath='xspawn zathura && xdotool key super+f'
