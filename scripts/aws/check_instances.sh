#!/usr/bin/env bash

set -eo pipefail

usage() {
    echo "check_instances.sh -r region [reservation_id-1 reservation_id-2 ...]"
}

output_all_instances_for_region() {
    local region_name="${1}"
    aws ec2 describe-spot-instance-requests \
        --filters "Name=state,Values=active" \
        --query "SpotInstanceRequests[*].[InstanceId]" \
        --output text \
        --region "${region_name}"
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

    if [[ $# -eq 0 ]]; then
        echo "Missing spot request id, giving all instances at ${region_name}"
        output_all_instances_for_region "${region_name}"
        exit 0
    fi

    for request_id in "${@}"; do
        aws ec2 describe-spot-instance-requests \
            --filters "Name=spot-instance-request-id,Values=${request_id}" \
                "Name=state,Values=active" \
            --query "SpotInstanceRequests[*].[InstanceId]" \
            --output text \
            --region "${region_name}"
    done
}

run "$@"
