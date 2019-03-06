if [[ -z "${ZPROFILE_HAS_BEEN_SOURCED}" ]]; then
    [[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'
    export ZPROFILE_HAS_BEEN_SOURCED=1
fi
