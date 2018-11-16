#!/bin/bash

read -r -d '' doc << EOM
{% START INSERT HERE %}
EOM

# ---------- Modules ----------
source gutils.sh

# ---------- Command-line Arguments ----------
eval set -- "$(getopt -o "d,h,v" -l "debug,help,verbose" -- "$@")"

export USAGE_GRAMMAR=(
    "[-d] [-v]"
    "[-h]"
)

# shellcheck disable=SC2154
read -r -d '' help << EOM
$(usage)

${doc}

Optional Arguments:
    -d | --debug
        Enable debug mode.

    -v | --verbose
        Enable verbose output.

    -h | --help
        View this help message.
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
