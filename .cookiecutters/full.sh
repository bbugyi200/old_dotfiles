#!/bin/bash

read -r -d '' doc << EOM
{% INSERT %}
EOM

# ---------- Modules ----------
source gutils.sh

# ---------- Command-line Arguments ----------
eval set -- "$(getopt -o "d,h" -l "debug,help" -- "$@")"

export USAGE_GRAMMAR=(
    "[-d]"
    "-h"
)

# shellcheck disable=SC2154
read -r -d '' help << EOM
$(usage)

${doc}

Optional Arguments:
    -d | --debug
        Enable debug mode.

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
        -- )
            shift
            break
            ;;
    esac
    shift
done

if [[ "${debug}" = true ]]; then
    PS4='$LINENO: '
    set -x
fi

# ---------- Main ----------
main() {
    :
}


if [[ "${SCRIPTNAME}" == "$(basename "${BASH_SOURCE[0]}")" ]]; then
	main "$@"
fi
