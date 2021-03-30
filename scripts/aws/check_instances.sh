#!/usr/bin/env bash

set -eo pipefail

usage() {
    echo "check_instances.sh -r region reservation_id-1 reservation_id-2 ..."
}

run() {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local region_name
    while getopts ":r:h" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            r)
                region_name="${OPTARG}"
                ;;
            :)
                echo "Option -${OPTARG} requires an argument"
                usage
                exit 1
                ;;
            *)
                echo "Unrecognized option -${OPTARG}"
                usage
                exit 1
                ;;
        esac
    done

    shift $((OPTIND - 1))

    for request_id in "${@}"; do
        while true; do
            # echo "Waiting for instances of ${request_id} to become active"
            ids=$(aws ec2 describe-spot-instance-requests \
                  --filters "Name=spot-instance-request-id,Values=${request_id}" \
                      "Name=state,Values=active" \
                  --query "SpotInstanceRequests[*].[InstanceId]" \
                  --output text \
                  --region "${region_name}")
            if [[ "$?" != "0" ]]; then
                echo "Bad ids ${ids}"
                exit 1
            else
                # echo "Id ${ids}"
                echo "${ids}"
                break
            fi
        done
    done
}

run "$@"
