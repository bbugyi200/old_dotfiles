#!/bin/bash

read -r -d '' DOC <<EOM
{% INSERT %}
EOM

source bugyi.sh

main() {
    parse_cli_args "$@"
}

function parse_cli_args() {
    eval set -- "$(getopt -o "d,h" -l "debug,help" -- "$@")"

    export USAGE_GRAMMAR=(
        "[-d]"
        "-h"
    )

    # shellcheck disable=SC2154
    read -r -d '' help <<EOM
$(usage)

${DOC}

Optional Arguments:
    -d | --debug
        Enable debug mode.

    -h | --help
        View this help message.
EOM

    while [[ -n "$1" ]]; do
        case $1 in
        -d | --debug)
            debug=true
            ;;
        -h | --help)
            echo "${help}"
            exit 0
            ;;
        --)
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
}

if [[ "${SCRIPTNAME}" == "$(basename "${BASH_SOURCE[0]}")" ]]; then
    main "$@"
fi
