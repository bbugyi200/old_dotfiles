# Aliases and Functions for TaskWarrior

# ---------- TaskWarrior
tcs () {
    TARGET_CONTEXT="$1"; shift
    task rc.context="$TARGET_CONTEXT" "$@"
}

tcsn () { tcs none "$@"; }

tc () { clear && task next +READY limit:page; }

tcx () {
    if [[ -z "$1" ]]; then
        task context show
    else
        task context "$@" && task_refresh -F rename,config
    fi
}

ts () {
    task start.not: stop

    if [[ -n "$1" ]]; then
        # Hook will stop any started tasks (not needed here)
        task rc.verbose:nothing start $1
    else
        task
    fi
}

tin () { task +inbox -DELETED -COMPLETED all; }

# All functions that use 'to' REQUIRE their first argument to
# be an ID.
ti () { task rc._forcecolor:on "$@" info | less; }
tl () { task "$1" | less; }
tpi () { task "$1" mod -inbox "${@:2}"; }
tg () { eval "tcsn $@ rc.verbose=blank,label list"; }
tgw () { eval "tcsn $@ rc.verbose=blank,label waiting"; }
tga () { eval "tcsn rc.verbose=blank,label rc.defaultwidth:$COLUMNS $@ -COMPLETED -DELETED all"; }
tgcd () { eval "tcsn rc.verbose=blank,label $@ \( +COMPLETED or +DELETED \) all"; }
tget () { task _get "$@"; }
tnall () { tcsn "next +READY"; }
tnl () { task next +READY limit:none; }  # no limit
tsub () { task $1 modify "/$2/$3/g"; }
trev () { task rc.context:review rc.verbose:nothing rc.defaultwidth:$COLUMNS limit:none \( +PENDING or +WAITING \) | less; }


alias t='task'
alias ta='task add'
alias td='task done'
alias qtrev='trev'
alias tlat='task rc._forcecolor:on +LATEST info | less'
alias tdue='tga +OVERDUE'
alias tdel='task delete'
alias tcn='task context none && task_refresh -F rename,config'
alias tcl='task context low && task_refresh -F rename,config'
alias tcm='task context mid && task_refresh -F rename,config'
alias tch='task context high && task_refresh -F rename,config'
alias tcb='task context bedtime && task_refresh -F rename,config'
alias tcmob='task context mobile && task_refresh -F rename,config'
alias tcomp='task limit:10 \( status:completed or status:deleted \) rc.report.all.sort:end- all'

# ---------- TimeWarrior
twc () {
    clear
    timew summary from "6:00" to tomorrow :id 2> /dev/null
    if [[ "$?" -ne 0 ]]; then
        timew summary from "$(date --date='yesterday' +%Y-%m-%d)T06:00" to tomorrow :id
    fi
    timew
}

twm () {
    timew move @1 "$1" :adjust
}

alias tw='timew'
alias timd='tim delete'

# ---------- Khal
alias k='khal --color'
restart_khal_alarms() { setsid khal-alarms &> /dev/null; }
kc() { clear && khal list --notstarted --format '{start-time} {title}' now && echo; }
kn() { khal new "$*" && restart_khal_alarms; }
knt() { khal new tomorrow "$@" && restart_khal_alarms; }
ke() { khal edit "$@" && restart_khal_alarms; }
ki() { ikhal "$@" && restart_khal_alarms; }

# ---------- Jrnl
alias j='jrnl'
alias je='jrnl --edit'
alias js='jrnl secure'
alias jse='jrnl secure --edit'
