#!/bin/bash

read -r -d '' DOC <<EOM
{% INSERT %}
EOM

source bugyi.sh

function run() {
    setup_traps INT TERM

    local cli_args=("$@")
    parse_cli_args "$@"
    dmsg "Command-Line Arguments: (${cli_args[*]})"
}

function parse_cli_args() {
    eval set -- "$(getopt -o "h,v" -l "help,verbose" -- "$@")"

    export USAGE_GRAMMAR=(
        "[-v]"
        "-h"
    )

    # shellcheck disable=SC2154
    read -r -d '' HELP <<EOM || [[ -n "${HELP}" ]]
$(usage)

${DOC}

Optional Arguments:
    -h | --help
        View this help message.

    -v | --verbose
        Enable verbose output. This option can be specified multiple times (e.g. -v, -vv, ...).
EOM

    VERBOSE=0
    while [[ -n "$1" ]]; do
        case $1 in
        -h | --help)
            echo "${HELP}"
            exit 0
            ;;
        -v | --verbose)
            VERBOSE=$((VERBOSE + 1))
            ;;
        --)
            shift
            break
            ;;
        esac
        shift
    done

    if [[ "${VERBOSE}" -gt 1 ]]; then
        PS4='$LINENO: '
        set -x
    fi

    readonly DOC
    readonly HELP
    readonly VERBOSE
}

if [[ "${SCRIPTNAME}" == "$(basename "${BASH_SOURCE[0]}")" ]]; then
    run "$@"
fi
