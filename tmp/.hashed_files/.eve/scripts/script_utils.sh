#!/bin/bash

# This function implements a retry loop to safely use `wget` to download from the web.
function esafe_wget() {
    if [[ $- == *x* ]]; then
        local debug=true
        set +x
    fi

    local url="$1"
    shift

    local max_retries=10
    local retries_left="${max_retries}"
    while [[ "${retries_left}" -gt 0 ]]; do
        if wget "$@" "${url}"; then
            break
        fi

        local delay=$((2 ** (max_retries - retries_left)))
        if [[ "${delay}" -gt 32 ]]; then
            delay=32
        fi

        printf "I: Sleeping for %ds before retrying 'wget' download.\n" "${delay}"
        sleep "${delay}"

        retries_left=$((retries_left - 1))
    done

    if [[ "${debug}" = true ]]; then
        set -x
    fi
}
