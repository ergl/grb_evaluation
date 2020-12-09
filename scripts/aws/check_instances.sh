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

    local instance_ip
    for request_id in "${@}"; do
        while true; do
            sleep 2
            echo "Waiting for instances of ${request_id} to become active"
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
                instance_ip=$(aws ec2 describe-instances \
                    --instance-ids "${ids}" \
                    --query 'Reservations[*].Instances[*].PublicIpAddress' \
                    --output text \
                    --region "${region_name}")
                echo "Id ${ids} / IP ${instance_ip}"
                break
            fi
        done
    done
}

run "$@"