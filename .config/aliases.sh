#!/bin/bash

#################################
#  Shell Aliases and Functions  #
#################################

# shellcheck disable=SC1010
# shellcheck disable=SC2009
# shellcheck disable=SC2079
# shellcheck disable=SC2142
# shellcheck disable=SC2154
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
robinit() { DATE="$(date +%Y%m%d)" cookie robot.yaml -f "$HOME"/.local/share/red_robot/pending/"$1"; }
pytinit() { SCRIPT="$1" cookie pytest_script.py -f test_"$1".py; }
alias texinit='cookie template.tex -f'

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
tdi () { task "$(tnext_inbox_id)" done; }
alias tdue='tga +OVERDUE'
tga () { eval "tcsn rc.verbose=blank,label rc.defaultwidth:$COLUMNS $* -COMPLETED -DELETED all"; }
tget () { task _get "$@"; }
tgp () { eval "tga project:$*"; }
tgps () { eval "tgp Study.$*"; }
tgs() { tga project:Study."$*"; }
tgw () { eval "tcsn $* rc.verbose=blank,label waiting"; }
ti () { task rc._forcecolor:on "$@" info | less; }
tin () { task rc.context=none +inbox -DELETED -COMPLETED all; }
tl () { task "$1" | less; }
alias tlat='task rc._forcecolor:on +LATEST info | less'
tnall () { tcsn "next +READY"; }
tnl () { task next +READY limit:none; }  # no limit
tpa () { tga project:"$(tproject)"; }
trev () { task rc.context:review rc.verbose:nothing rc.defaultwidth:$COLUMNS limit:none \( +PENDING or +WAITING \) | less; }
alias tstudy='vim ~/.vimwiki/TaskWarrior.wiki'
tsub () { task "$1" modify "/$2/$3/g"; }
alias tw='timew'
alias wdel='if ! watson_is_on; then watson remove -f $(watson frames | tail -n 1); else return 1; fi'
alias wedit='watson edit'
alias wlog='watson log'
alias wstat='watson status'

# ---------- Mutt Aliases / Functions ----------
# def marker: MUTT
alias bmutt='TERM=rxvt-unicode-256color neomutt -f /home/bryan/.mail'
alias mutt="TERM=rxvt-unicode-256color neomutt"
alias rmutt="TERM=rxvt-unicode-256color neomutt -e 'source ~/.mutt/hooks/bmb181@scarletmail.rutgers'"
alias sudo-mutt='TERM=rxvt-unicode-256color sudo neomutt -f /var/spool/mail/root'
alias vmutt='vim $HOME/.mutt/muttrc'

# ---------- Vim Wrapper Aliases / Functions ----------
# def marker: VIM
cim() { vim ~/.config/"$1"; }
alias daf='def -a'
def() { zim "def" "$@" "-F" "$HOME/Sync/home/.zshrc" "-F" "$HOME/Sync/home/.config/aliases.sh" "-F" "$HOME/Sync/home/.config/debian.sh" "-F" "$HOME/Sync/home/.config/gentoo.sh" "-F" "$HOME/Sync/home/.config/macos.sh"; }
him() { vim ~/"$1"; }
lim() { vim -c "normal '0" -c 'bd1'; }
mim() { zim "mim" "$@"; }
tam() { N="$(history -n | tail -n 100 | tac | nl | fzf --tiebreak=index | awk '{print $1}')"; if [[ -n "${N}" ]]; then tim "${N}" "$@"; fi; }
tim() { f=$(fc -e - -"${1:-1}" 2> /dev/null | fzf -q "$2"); if [[ -n "${f}" ]]; then vim "${f}"; fi; }
alias v='vim'
alias wam='wim -a'
wim() { zim wim "$@"; }
zim() { "$HOME"/.local/bin/zim "$@" || { EC="$?"; if [[ "${EC}" -eq 3 ]]; then so; else return "${EC}"; fi; }; }

# ---------- File Copy / Cut / Paste ----------
p() { echo "The following files have been pasted into ${PWD}/${1}:" && ls -A /tmp/copy && /bin/mv -i /tmp/copy/* "${PWD}"/"${1}"; }
x() { mkdir /tmp/copy &> /dev/null; /bin/mv "$@" /tmp/copy/; }
y() { mkdir /tmp/copy &> /dev/null; /bin/cp -r "$@" /tmp/copy/; }

# ---------- Salary ----------
daily_salary() { printf "%f\n" $(($(weekly_salary "$1") / 5.0)); }
hourly_salary() { printf "%f\n" $(($(weekly_salary "$1") / 40.0)); }
hsal() { sal "$(($1 * 40.0 * 52.0 / 1000.0))" "${@:2}"; }
monthly_salary() { printf "%f\n" $(($(yearly_salary "$1") / 12.0)); }
NET_P=$((1.0 - TAX_P))
sal() { clear && salary "$@" && echo; }
salary() { printf "======= BEFORE TAXES =======\n" && _salary "$1" 0 && printf "\n===== AFTER TAXES (%0.1f%%) =====\n" "${2:-$((TAX_P * 100.0))}" && _salary "$@"; }
_salary() { { [[ -n "$2" ]] && NET_P=$((1.0 - ($2 / 100.0))); }; printf "Hourly:       $%0.2f\nDaily:        $%0.2f\nWeekly:       $%0.2f\nBiweekly:     $%0.2f\nSemi-monthly: $%0.2f\nMonthly:      $%0.2f\nYearly:       $%0.2f\n" "$(hourly_salary "$1")" "$(daily_salary "$1")" "$(weekly_salary "$1")" "$((2 * $(weekly_salary "$1")))" "$((0.5 * $(monthly_salary "$1")))" "$(monthly_salary "$1")" "$(yearly_salary "$1")"; NET_P=$((1.0 - TAX_P)); }
TAX_P=0.285;  # Default tax percentage used for salary calculation.
weekly_salary() { printf "%f\n" $(($(yearly_salary "$1") / 52.0)); }
yearly_salary() { printf "%f\n" $(($1 * 1000.0 * NET_P)); }

# ---------- Miscellaneous Aliases / Functions ----------
# def marker: DEFAULT
alias activate='source venv/bin/activate'
addgroup() { sudo usermod -aG "$1" bryan; }
alias ag='ag --hidden'
alias anki='xspawn anki'
auto() { nohup autodemo "$@" &> /dev/null & disown && clear; }
bar() { i=0; while [[ $i -lt "$1" ]]; do printf "*"; i=$((i+1)); done; printf "\n"; }
bgdb() { gdb "$1" -ex "b $2" -ex "run"; }
alias books='vim ~/Sync/var/notes/Journal/books.txt'
box() { blen=$((4 + ${#1})); bar "${blen}"; printf "* %s *\n" "$1"; bar "${blen}"; }
alias budget='python3 $HOME/Sync/var/projects/budget/main.py'
alias bw='sudo bandwhich'
alias c='cookie'
cat() { if [[ -r "$1" ]]; then bat "$@"; else sudo -E bat "$@"; fi; }
alias ccat='pygmentize -g'
ccd() { cd "$HOME/.cookiecutters/$1/{{ cookiecutter.project|lower }}" &> /dev/null || return 1; }
alias cdef='def -m COOKIE'
alias cdow='cd "$(dow_dir $PWD)"'
cho() { sudo chown -R "${2:-bryan}":"${2:-bryan}" "$1"; }
alias chx='sudo chmod +x'
alias cower='cower -c'
alias cp="cp -i"
alias cplug='vim +PluginClean +qall'
alias cppinit='cinit ++'
cprof() { python -m cProfile -s "$@" | less; }
alias crun='cargo run --'
cval() { pushd "$1" &> /dev/null || return 1 && eval "$2"; popd &> /dev/null || return 1; }
alias d.='desk .'
alias d='mkdvtm'
alias dayplan='cd $HOME/Sync/var/notes && vim dayplan.txt'
dc() { sudo -E deluge-console "${@}"; }
dci() { dc info --sort=time_added | awk -F ':' "{if(\$1==\"$1\")print \$0}"; }
alias ddef='def -m DEBIAN'
alias ddwrt-logs='sim /var/log/syslog-ddwrt'
alias del_swps='find . -name "*.swp" -delete -print'
alias delshots='confirm "find $HOME/Sync/var/aphrodite-motion -name \"*$(date +%Y%m%d)*\" -delete"'
alias dff='df -x tmpfs -x squashfs -x devtmpfs -h'
dg() { { box "ALIAS DEFINITIONS"; alias | grep --color=never -E "=.*$1" | grep --color=always -E "$1"; printf "\n" && box "FUNCTION DEFINITIONS" && typeset -f | ${SED} '/^$/d' | ${SED} '/^_.\+ () {/,/^}$/d' | ${SED} 's/^}$/}\n/g' | grep --color=never -E " \(\) |$*" | ${SED} '/--$/d' | grep --color=never -B 1 -E "$1[^\(]*$" | grep --color=never --invert-match -E "$1.*\(\)" | grep -B 1 -E "$1" --color=never | ${SED} 's/ {$/:/g' | ${SED} '/--$/d' | ${SED} 'N;s/\:\n/: /g' | ${SED} 's/ ()\:\s*/(): /g' | grep -E "(): " | grep --color=always -E "$@"; printf "\n"; box "SCRIPT CONTENTS"; rg -s -C 5 -p "$@" ~/Sync/bin; }; }
dgw() { dg "\W$1\W"; }
diff() { colordiff -wy -W "$(tput cols)" "$@" | less -R; }
alias dst='dropbox-cli status'
alias dstart='dropbox-cli start'
alias dstop='dropbox-cli stop'
alias du='sudo ncdu --color dark'
alias dunst='killall dunst &> /dev/null; dunst'
alias edsl='printf "$(hostname):%d,%d\n%s,%d\n" $(emanage -D local -u) $(emanage -D local -c) $(emanage -D remote -u) $(emanage -D remote -c | awk -F: "{print \$2}")'
alias epuse='sudo -E epuse'
_essh() { printf 'cd ~/projects/edgelp/prod; source envs/dev.sh; cd %s; /bin/zsh' "$1"; }
essh() { ssh "$1" -t "$(_essh "$2")"; }
esssh() { essh "$1" /prod/home/bbugyi/src/prod; }
fim() { file="$("$(which -a fim | tail -n 1)" "$1")"; if [[ -z "${file}" ]]; then return 1; else vim "${file}"; fi; }
alias flaggie='sudo -i flaggie'
alias fn='noglob fn_'
fn_() { if [[ "$1" == *"*"* ]]; then find . -iname "$@"; else find . -iname "*$**"; fi; }
forever() { while true; do eval "$*"; done; }
alias fp='noglob fp_'
fp_() { if [[ "$1" == *"*"* ]]; then find . -ipath "$@"; else find . -ipath "*$**"; fi; }
alias freeze='icebox --freeze /tmp/icebox'
alias ga='git add -v'
alias gaa='git add -v --all'
alias gau='git add -v --update'
alias gbb='git branch --sort=-committerdate | less'
alias gbcopy='gcopy --body'
gca() { if [[ -n "$1" ]]; then git commit -v -a -m "$1"; else git commit -v -a; fi; }
gcB() { gbD "$1" &> /dev/null; git checkout -b "$1" "${2:-upstream}"/"$1"; }
gcbc() { git checkout -b "$@" && git commit --allow-empty; }
gcbd() { if [[ -z "$1" ]]; then return 1; fi; gcb "$(date +"%y.%m")"-"$1"; }
alias gce='git commit --allow-empty'
alias gcignore='git add .gitignore && git commit -m "Update: .gitignore file"'
gcl() { cd "$("$HOME"/.local/bin/gcl "$@")" || return 1; }
alias gclp='cd ~/projects && gcl'
alias gclt='cd /tmp && gcl'
gcm() { git checkout "${MASTER_BRANCH:-master}"; }
gcopys() { gcopy --body "$@" &>/dev/null && gcopy --title "$@" &>/dev/null; }
alias gdef='def -m GENTOO'
alias Gdef='def -m GTD'
gdm() { git diff "${MASTER_BRANCH:-master}"...HEAD; }
alias gdo='git diff origin/master'
alias geff='git effort'
alias gg='git grep --break --heading'
alias gga='git rev-list --all | xargs git grep -n --break --heading'
alias gho='ghi open'
alias ghooks='rm -rf .git/hooks && git init' 
alias gi='git info -c 3 --no-config'
alias ginit='while true; do; watch -d -n 1 cat .gdbinit; vim .gdbinit; done'
git() { if [[ -n "$1" ]]; then command git "$@"; else tig; fi; }
git_issue_number() { git branch | grep '^\*' | awk '{print $2}' | awk -F'-' '{print $1}'; }
alias glg++='glg ++'
alias glg+='glg +'
alias Glg='git log -p -G'
alias glog='git log'
alias gmc='git ls-files -u | cut -f 2 | sort -u'
gN() { git checkout HEAD~"${1:-1}"; }
gN1() { git_current_branch > /tmp/gnext-branch.txt && gN "$@"; }
alias gn='gnext'
alias gpa='git commit -v -a --no-edit --amend && git push --force'
alias gpf='git push --force'
alias gprm='gpup "Docs: Update README"'
gpu() { git push -u origin "$(git_current_branch)"; }
alias gpull='git stash && git pull && git stash apply'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gre='git restore'
alias grep='${GREP}'
alias gres='git reset'
gresh() { git reset "${@:2}" HEAD~"${1:-1}"; }
greshh() { gresh "${1:-0}" --hard; }
greshs() { gresh "${1:-1}" --soft; }
alias grest='git restore'
alias grests='git restore --staged'
alias grl='git reflog'
grun() { [[ "$(tail -n 1 "${PWD}"/.gdbinit)" == "r" ]] && sed -i '/^r$/d' "${PWD}"/.gdbinit || printf "r\n" >> "${PWD}"/.gdbinit; }
alias gsd='sudo get-shit-done'
alias gsta='git stash'
alias gstal="git stash list --date=local | perl -pE 's/stash@\{(?<date>.*)\}: .*[Oo]n (?<branch>.*?): (?<desc>.*)$/\"\\033[33mstash@\{\" . \$n++ . \"\}: \\033[36m[\$+{date}] \\033[32m(\$+{branch})\n\t\$+{desc}\n\"/ge' | less"
alias gsum='git summary | less'
alias gtcopy='gcopy --title'
gwip() { gaa && git commit -m "[wip] $*"; }
alias h='tldr'
alias H='tm-home load'
header() { clear && eval "$@" && echo; }
help() { bash -c "help $*"; }
alias htime='hyperfine'
alias htop='sudo htop'
info() { pinfo "$@" || { printf "\n===== APROPOS OUTPUT =====\n"; apropos "$@"; }; }
alias iotop='sudo iotop'
alias ipdb='ipdb3'
alias iplug='vim +PluginInstall +qall'
ipy-add-import() { ${SED} -i "/c\.InteractiveShellApp\.exec_lines/ a import $1" ~/.ipython/profile_default/ipython_config.py; }
alias issh='ssh -p 34857 athena-arch.ddns.net'
ivim() { while true; do vim "$@" && sleep 0.5; done; }
ivimbc() { while true; do vim $(branch_changes | sort_by_basename | perl -nE 'print if not /thirdparty/') && sleep 0.5; done; }
j() { if [[ -n "$1" ]]; then jrnl "$@"; else vim + "$HOME"/Sync/var/notes/Journal/jrnl.txt; fi; }
alias J='pushd ~/Sync/var/notes/Journal &> /dev/null && ranger && popd &> /dev/null'
K() { tmux switchc -n && tmux kill-session -t "$(tm-session-name)"; }
alias k9='sudo kill -9'
Kman() { man -wK "$@" | awk -F'/' '{print $NF}' | sed 's/\.\(.*\)\.bz2$/ (\1)/g' | sort; }
alias kman='man -k'
alias loc='locate --regex'
alias Loc='sudo updatedb && loc'
alias lpass-login='lpass login bryanbugyi34@gmail.com'
alias ls='exa -g'
alias lt='ls --tree'
m-torrent() { echo "torrent -w /media/bryan/hercules/media/Entertainment/Movies $*" | at 0200; }
alias matlab='matlab -nojvm -nodisplay -nosplash'
alias mirror='xrandr --output DVI-I-1-1 --auto --same-as LVDS1'
mkcd() { mkdir -p "$1" && cd "$1" || return 1; }
alias mkdir='mkdir'
alias mkgrub='sudo grub-mkconfig -o /boot/grub/grub.cfg'
alias mkpkg='makepkg -si'
alias mpvlc='xspawn -w mpv mpvlc'
alias mrun='macrun'
alias multivisor-cli='multivisor-cli --url athena:8100'
alias mv="mv -i"
alias myip='ip addr | grep -P -o "192.168.1.[0-9]+" | grep -v 192.168.1.255'
alias noeye='eye --purge-eye'
alias nomirror='xrandr --output DVI-I-1-1 --auto --right-of LVDS1'
alias notes='pushd ~/Sync/var/notes/Journal &> /dev/null && ranger && popd &> /dev/null'
alias ok='xspawn okular'
onething() { vim -c "/$(date --date="yesterday" +%m\\/%d\\/%Y)" ~/Sync/var/notes/Onething/"$1".txt; }
alias P='popd'
pdb() { { [[ -f ./"$1" ]] && python -m pdb "$@"; } || python -m pdb "$(which -a "$1" | tail -n 1)" "${@:2}"; }
pgr() { pgrep -f ".*$1.*"; }
alias pipget='pip install --user'
alias plex='xspawn -w plex plexmediaplayer'
pname() { pass show | grep -i "$1" | awk '{print $2}'; }
ppg() { if [[ -n "$1" ]]; then pipenv graph | grep "$@"; else pipenv graph; fi; }
alias ppi2='pipenv --two install'
alias ppi='pipenv install'
alias ppr='pipenv run'
alias ppu='pipenv uninstall'
ppython() { pipenv run python "$@"; }
alias prun='poetry run'
alias psg='ps -aux | grep -v grep | grep'
alias pshell='poetry shell'
alias psi='psinfo'
alias pstrace="strace \$@ -p \$(ps -ax | fzf | awk '{print \$2}')"
pudb() { { [[ -f ./"$1" ]] && pudb3 "$@"; } || pudb3 "$(which -a "$1" | tail -n 1)" "${@:2}"; }
pvar() { set | grep -i -e "^$1"; }
alias pvsu='py-vshlog -u -D BOT EOT -H all -e'
alias pwrstat='sudo pwrstat'
pycov() { coverage run "$1" && coverage html && qutebrowser htmlcov/index.html; }
alias Q='tm-kill'
alias q='{ sleep 0.1 && tm-fix-layout; } & disown && exit'
alias rag='cat $RECENTLY_EDITED_FILES_LOG | sudo xargs ag 2> /dev/null'
alias reboot='sudo reboot'
ripmov() { nohup torrent -dv -w /media/bryan/hercules/media/Entertainment/Movies "$@" &> /dev/null & disown; }
riptv() { nohup torrent -dv -w /media/bryan/hercules/media/Entertainment/TV "$@" &> /dev/null & disown; }
alias rm='trash'
alias rng='ranger'
alias root='sudo su -p'
alias rrg='cat "$RECENTLY_EDITED_FILES_LOG" | sudo xargs rg 2> /dev/null'
alias sat='sudo cat'
alias sc='systemctl'
alias sch='vim ~/Sync/var/notes/Rutgers/course_schedule.txt'
alias scu='systemctl --user'
alias sftp-rutgers='sftp bmb181@less.cs.rutgers.edu'
alias sim='sudo -E vim'
alias snapshots='find $HOME/Sync/var/aphrodite-motion -name "*$(date +%Y%m%d)*" | sort | xargs imv && delshots'
alias sqlite3='rlwrap -a -N -c -i sqlite3'
SS() { tmux send-keys "sleep 1.5 && !-2" "Enter"; }
alias ssh-aphrodite='ssh 192.168.1.193'
alias ssh-artemis="ssh bryan@67.207.92.152"
alias ssh-athena-tm='ssh-athena /home/bryan/.local/bin/tm Terminal'
ssh-rutgers() { ssh bmb181@"${1:-less}".cs.rutgers.edu; }
alias su='su -p'
alias sudo='sudo -E '  # makes aliases visible to sudo
alias sudoers='sudo -E vim /etc/sudoers'
alias supctl='supervisorctl -c /home/bryan/.config/supervisor/supervisord.conf'
alias tgdb="gdb -iex 'set pagination off' -ex 'tui enable' -ex 'set pagination on'"
alias tm-layout="tmux lsw | grep '*' | awk '{gsub(/\\]/, \"\"); print \$7}'"
tmd() { tmux display-message -p "#{$1}"; }
alias todo='rg "^[ ]*..?[ ]TODO\(b?bugyi\):[ ].*$" -l --color=never | sort_by_basename | pytodos'
alias tree='clear && tree -I "venv*|__pycache__*|coverage*"'
tsm-add() { transmission-remote -a "$@"; }
tsm-boost() { transmission-remote -t"$1" -Bh -phall -pr250; }
tsm-mov() { tsm-add "$@" -w "$MOV"; }
tsm-purge() { transmission-remote -t"$1" -rad; }
tsm-rm() { transmission-remote -t"$1" -r; }
tsm-start() { sudo service transmission-daemon start; }
tsm-stop() { sudo service transmission-daemon stop; }
tsm-tv() { tsm-add "$@" -w "$TV"; }
tsm-watch() { watch -n 1 transmission-remote -l; }
alias tsm='transmission-remote'
tv-torrent() { echo "torrent -w /media/bryan/hercules/media/Entertainment/TV $*" | at 0200; }
u() { echo -e "\u$1"; }
alias undow='dow --undo'
alias unfreeze='icebox --unfreeze /tmp/icebox'
alias updatedb='sudo updatedb'
alias uplug='vim +PluginUpdate +qall'
vab() { vim $(find "$HOME"/Sync/bin/cron.jobs -type f | sort | tr '\n' ' '); }
alias valg='valgrind --leak-check=full --show-reachable=yes --track-origins=yes'
alias vbox='xspawn sudo virtualbox'
alias vbt='vim ~/.local/share/torrent/*.txt'
alias vcron='vim ~/Sync/bin/cron.jobs/jobs.sh ~/Sync/bin/cron.jobs/{cron.hourly/hourly_jobs,cron.daily/daily_jobs,cron.weekly/weekly_jobs} ~/Sync/bin/cron.jobs/backup.sh ~/Sync/bin/cron.jobs/cron.{daily,weekly,monthly}/*'
alias vdaily="vgtd-daily-review"
alias vdb='vim $HOME/Sync/bin/cron/cron.daily/*'
alias vdiff='vimdiff -n'
venv() { vim "$HOME"/.zprofile "$HOME"/.profile "$HOME"/Sync/etc/environment "$(find "$HOME"/Sync/etc/profile.d -type f)" "$HOME"/.local/bin/etc-generator; }
alias vgdb-l='voltron view command "cmds set listsize $(tput lines) ; list *\$pc" --lexer c'
alias vgdb='vim ~/.gdbinit .gdbinit'
Vgi() { if [[ -f ./.local_gitignore ]]; then vim -c 'vs ./.local_gitignore' ~/.gitignore_global; else vim ~/.gitignore_global; fi; }
alias vgutils='vim /usr/bin/gutils.sh'
alias vihor='vim ~/Sync/var/notes/Horizons_of_Focus/*'
alias vimilla='vim -u ~/.vanilla-vimrc'
alias vipy='vim -c "/c.InteractiveShellApp.exec_lines" ~/.ipython/profile_default/ipython_config.py'
alias vm='vman'
alias vmb='vim $HOME/Sync/bin/cron/cron.monthly/*'
alias vmkrules='make -p > /tmp/make-rules && vim /tmp/make-rules'
alias vnc-athena='open vnc://athena-arch.ddns.net:34590'
alias vnix='vv_push ~/.nixnote'
alias vpyutils='pushd ~/Sync/lib/python/gutils &> /dev/null && vv && popd &> /dev/null'
alias vq='vv_push ~/.config/qutebrowser'
alias vr='vim ${RECENTLY_EDITED_FILES_LOG}'
alias vrf='vv_push ~/Sync/bin/main/rfuncs'
vrobot() { vim "$HOME"/.local/share/red_robot/pending/"$1"; }
alias vs='vshlog'
alias vscratch='vim ~/Sync/var/notes/scratch.txt'
alias vsd='vshlog -H all -D'
alias vstudy='vim $HOME/.vimwiki/TaskWarrior.wiki'
alias vsup='vim /etc/supervisor/supervisord.conf ~/.config/supervisor/supervisord.conf ~/.config/supervisor/*'
alias vtorr='cval "$HOME/Sync/bin/main" "vim torrent libtorrent/**/[^_]*.py"'
alias vtv="vim \$HOME/.local/bin/tmux_view.sh \$HOME/.local/bin/tv_*"
vuse() { vim /etc/portage/package.use/"$1"; }
vv_push() { tmux send-keys "clear && pushd '$1' &> /dev/null && vv && popd &> /dev/null && clear" "Enter"; }
alias vwb='vim $HOME/Sync/bin/cron/cron.weekly/*'
alias vweekly='vgtd-weekly-review'
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
alias wttr='clear && curl "wttr.in/?T"'
alias wwat='watch -n 1 "{ wpoll; echo; watson log; }"'
alias xc='xclip -sel clipboard'
alias xdokey='xev -event keyboard'
alias xk='xdokey'
alias xkey='xdotool key'
alias xmonad-keycodes='vim /usr/include/X11/keysymdef.h'
alias xs='xspawn'
ytd() { pushd "${HOME}"/Downloads &> /dev/null || return 1 && youtube-dl "$(xclip -sel clipboard -out)" --output "$1"; popd &> /dev/null || return 1; }
alias zath='xspawn zathura && xdotool key super+f'
