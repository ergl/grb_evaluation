#!/usr/bin/env bash

set -eo pipefail

doForNodes() {
    if [[ $# -eq 0 ]]; then
        echo "execute-in-nodes.sh -i identity_file <command> <node-ips>"
    fi

    local key
    while getopts ":i:" opt; do
        case $opt in
            i) key="${OPTARG}" ;;
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
        ssh -i "${key}" -T \
          -o ConnectTimeout=3 \
          -o StrictHostKeyChecking=no \
          ubuntu@"${node}" "${command}"
    done

}

doForNodes "$@"
