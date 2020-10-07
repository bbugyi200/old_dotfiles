#!/usr/bin/env bash

# shellcheck disable=SC2164

if [[ -n "${PROD_DESK_ACTIVATED}" ]]; then
    2>&1 printf "[ERROR] The 'prod' desk environment is already activated.\n"
    exit 1
else
    export PROD_DESK_ACTIVATED=1

    old_pwd="$(pwd)"
    if [[ "$(basename "${old_pwd}")" != "prod" ]]; then
        2>&1 echo "ERROR: Must be in prod directory to used this desk workspace."
        return 1
    fi

    # These two lines should be able to be moved once
    # https://github.com/pyenv/pyenv/pull/1655 is resolved. You also had to run
    # `sudo ln -sf /usr/bin/python2.7 /usr/bin/python` for similar reasons.
    export PATH="/home/bryan/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"

    poetry_venv="$(pyenv shell 2.7.18 && poe show -v | perl -nE 'print s/Using virtualenv:\s*//gr if /Using virtualenv:/')"
    source "${poetry_venv}"/bin/activate

    export PATH=$(pwd)/.bin:$(pwd)/docker:"$PATH"
    source psource.sh
    
    if command -v eselect &> /dev/null; then
        old_python="$(eselect python show)"
        sudo eselect python set python2.7
    fi

    cd ~/projects/edgelp/prod
        psource .environ -a
    cd "${old_pwd}"

    if command -v eselect &> /dev/null; then
        sudo eselect python set "${old_python}"
    fi

    source pychecks.sh
    source .tmp/env
fi
