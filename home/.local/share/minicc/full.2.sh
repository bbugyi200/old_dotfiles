#!/bin/bash

read -r -d '' doc << EOM
{# START #}
EOM

# ---------- Modules ----------
source /home/bryan/Dropbox/scripts/modules/bash/gutils.sh

# ---------- Command-line Arguments ----------
eval set -- "$(getopt -o "d,h,v" -l "debug,help,verbose" -- "$@")"

scriptname="${scriptname}"
usage="${usage}"

# shellcheck disable=SC2154
read -r -d '' help << EOM
${usage^}

${doc}
EOM

while [[ -n "$1" ]]; do
    case $1 in
       -d|--debug )
           debug=true
           ;;
       -h|--help )
           echo "${help}"
           exit 0
           ;;
       -v|--verbose )
           verbose=true
           ;;
       -- )
           shift
           break
           ;;
    esac
    shift
done

if [[ "${debug}" = true && "${verbose}" = true ]]; then
    PS4='$LINENO: '
    set -x
fi

# ---------- Main ----------
