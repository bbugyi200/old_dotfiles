# Aliases and Functions for TaskWarrior

# ---------- TaskWarrior
tcs () {
    TARGET_CONTEXT="$1"; shift
    task_send "rc.context=$TARGET_CONTEXT $@"
}

tcsn () { tcs none "$@"; }

tt () {
    if [[ -n "$1" ]]; then
        task_send "$@"
    fi
    task_refresh
}

tc () {
    if [[ -z "$1" ]]; then
        task context show
    else
        task context "$@"
    fi
}

ts () {
    task start.not: stop

    if [[ -n "$1" ]]; then
        # Hook will stop any started tasks (not needed here)
        tt rc.verbose:nothing start $1
    else
        tt
    fi
}

tin () {
    clear

    if [[ -n "$1" ]]; then
        task_send "$@"
    fi

    task_send +inbox -DELETED -COMPLETED all
}

# All functions that use 'to' REQUIRE their first argument to
# be an ID.
to () { eval "task_send $@ && task next +READY; task $2 | less"; }
ti () { eval "task_send $1 info"; }
tpi () { task "$1" mod -inbox "${@:2}" && confirm "clear && task next +READY"; }
tmi () { eval "tin $1 mod ${@:2}"; }
tsi () { eval "tt add +inbox $@ && tt +LATEST start"; }
tg () { eval "tcsn $@ rc.verbose=blank,label list"; }
tgw () { eval "tcsn $@ rc.verbose=blank,label waiting"; }
tgr () { eval "tcsn status:Recurring $@ rc.verbose=blank,label all"; }
tga () { eval "tcsn rc.verbose=blank,label $@ -COMPLETED -DELETED all"; }
tgd () { eval "tcsn rc.verbose=blank,label $@ \( +COMPLETED or +DELETED \) all"; }
tget () { task _get "$@"; }
tall () { task_send; }
tnall () { tcsn "next +READY"; }
tnl () { task next +READY limit:none; }  # no limit
tsub () { tt $1 modify "/$2/$3/g"; }
trev () { tcs "review rc.defaultwidth:$COLUMNS rc.report.all.sort:urgency- \( +PENDING or +WAITING \) all limit:none"; }


alias t='task'
alias ta='task add'
alias tan='to annotate'
alias tu='to modify'
alias td='tt rc.verbose=nothing done'
alias tdo='task done'
alias tdel='tt delete'
alias tcn='tt context none'
alias tcl='tt context low'
alias tcm='tt context mid'
alias tch='tt context high'
alias tcomp='task limit:10 \( status:completed or status:deleted \) rc.report.all.sort:end- all'

# ---------- TimeWarrior
tim () {
    if [[ -n "$1" ]]; then
        timew "$@"
    else
        timew summary from "6:00" to tomorrow :id 2> /dev/null
        if [[ "$?" -ne 0 ]]; then
            timew summary from "$(date --date='yesterday' +%Y-%m-%d)T06:00" to tomorrow :id
        fi
    fi
}

timv () {
    timew move @1 "$1" :adjust
}

alias timd='tim delete'

# ---------- Khal
alias k='khal'
restart_khal_alarms() { kill "$(cat /tmp/khal-alarms.pid 2> /dev/null)" &> /dev/null; setsid khal-alarms &> /dev/null; }
kc() { clear && khal list --notstarted --format '{start-time} {title}' now && echo; }
kn() { khal new "$@" && kc && restart_khal_alarms; }
knt() { khal new tomorrow "$@" && kc && restart_khal_alarms; }
ke() { khal edit "$@" && kc && restart_khal_alarms; }

# ---------- ZSH Completions
compdef _task tt ti tpi ts to ta tg tgw tgr tga tin tmi tget

# vim: ft=zsh:
