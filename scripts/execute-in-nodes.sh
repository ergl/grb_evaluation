#!/usr/bin/env bash

set -eo pipefail

doForNodes() {
    if [[ $# -eq 0 ]]; then
        echo "execute-in-nodes.sh [-dp] <command> <node-glob>"
    fi

    local dry_run=0
    local par_for=0
    while getopts ":dp" opt; do
        case $opt in
            d)
                dry_run=1
                ;;
            p)
                par_for=1
                ;;
            *)
                echo "Unrecognized option -${OPTARG}"
                exit 1
                ;;
        esac
    done

    shift $((OPTIND - 1))

    local command="${1}"

    shift

    # same as `for node in "$@";`
    for node do
        if [[ ! "${node}" =~ "imdea" ]]; then
            echo "ERROR: Use fully qualified name for node ${node}"
            exit 1
        fi

        if [[ "${dry_run}" -eq 1 ]]; then
        echo ssh -i "${HOME}/.ssh/imdea_id_ed25519" -T \
                -o ConnectTimeout=3 \
                -o StrictHostKeyChecking=no \
                borja.deregil@"${node}" "${command}"
        elif [[ "${par_for}" -eq 1 ]]; then
            ssh -i "${HOME}/.ssh/imdea_id_ed25519" -T \
                -o ConnectTimeout=3 \
                -o StrictHostKeyChecking=no \
                borja.deregil@"${node}" "${command}" &
        else
            ssh -i "${HOME}/.ssh/imdea_id_ed25519" -T \
                -o ConnectTimeout=3 \
                -o StrictHostKeyChecking=no \
                borja.deregil@"${node}" "${command}"
        fi
    done

}

doForNodes "$@"
