################################## Aliases and Functions for GTD ##################################

ka() { /usr/local/bin/ka "$@" && krestart_alarms; }
kc() { clear && khal calendar --notstarted --format '{start-time} {title}' now && echo; }
ke() { khal edit "$@" && krestart_alarms; }
ki() { ikhal "$@" && krestart_alarms; }
kn() { khal new -a daily ""$*"" && krestart_alarms; }
knt() { khal new -a daily tomorrow ""$*"" && krestart_alarms; }
krestart_alarms() { setsid calalrms -d &> /dev/null; }
alias m="neomutt"
alias mb='neomutt -f /var/spool/mail/bryan'
alias mg="neomutt -e 'source ~/.mutt/hooks/bryanbugyi34.gmail'"
alias mr='sudo neomutt -f /var/spool/mail/root'
alias mrc='vim /home/bryan/.mutt/muttrc'
alias p='pass'
alias ta='task add'
alias tcn='task context none && task_refresh -F rename,config'
tc () { clear && task next rc.verbose=blank,label rc.defaultwidth:$COLUMNS +READY limit:page; }
alias td='task done'
alias tdel='task delete'
tdi () { task "$(tnext_inbox_id)" done; }
alias tdue='tga +OVERDUE'
alias tcomp='task limit:10 \( status:completed or status:deleted \) rc.report.all.sort:end- all'
tcs () { task rc.context="$1" "${@:2}"; }
tcsn () { tcs none "$@"; }
tcx () { task context "$@" && task_refresh -F rename,config; }
tg () { eval "tcsn $@ rc.verbose=blank,label list"; }
tga () { eval "tcsn rc.verbose=blank,label rc.defaultwidth:$COLUMNS $@ -COMPLETED -DELETED all"; }
tgcd () { eval "tcsn rc.verbose=blank,label $@ \( +COMPLETED or +DELETED \) all"; }
tget () { task _get "$@"; }
tgp () { eval "tga project:$@"; }
tgps () { eval "tgp Study.$@"; }
tgw () { eval "tcsn $@ rc.verbose=blank,label waiting"; }
ti () { task rc._forcecolor:on "$@" info | less; }
tin () { task +inbox -DELETED -COMPLETED all; }
tl () { task "$1" | less; }
alias tlat='task rc._forcecolor:on +LATEST info | less'
tnall () { tcsn "next +READY"; }
tnl () { task next +READY limit:none; }  # no limit
tpa () { tga project:$(tproject); }
tsub () { task $1 modify "/$2/$3/g"; }
trev () { task rc.context:review rc.verbose:nothing rc.defaultwidth:$COLUMNS limit:none \( +PENDING or +WAITING \) | less; }
alias tw='timew'
