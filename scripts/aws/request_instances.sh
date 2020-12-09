#!/usr/bin/env bash

set -eo pipefail

usage() {
    echo "start_instances.sh [-hy] [options]"
    echo -e "  where options are:"
    echo -e "  -r <region>"
    echo -e "  -t <instance_type>"
    echo -e "  -i <image_id>"
    echo -e "  -k <key_name>"
    echo -e "  -s <security_group_id>"
    echo -e "  -n <instance_num>"
    echo -e "  -p <spot_price>"
}

run() {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local do_run=0

    local region_name
    local itype
    local image
    local key
    local sg
    local inum=1
    local spot_price=0
    while getopts ":yr:t:i:k:s:n:p:h" opt; do
        case $opt in
            y) do_run=1 ;;
            r) region_name="${OPTARG}" ;;
            t) itype="${OPTARG}" ;;
            i) image="${OPTARG}" ;;
            k) key="${OPTARG}" ;;
            s) sg="${OPTARG}" ;;
            n) inum="${OPTARG}" ;;
            p) spot_price="${OPTARG}" ;;
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

    local set_price=""
    if [[ "${spot_price}" -ne 0 ]]; then
        set_price="--spot-price ${spot-price}"
    fi

    local spec_file
    spec_file=$(mktemp -t aws_start_instance)

    {
        echo "{"
        echo "\"ImageId\": \"$image\","
        echo "\"KeyName\": \"$key\","
        echo "\"SecurityGroupIds\": [ \"$sg\" ],"
        echo "\"InstanceType\": \"$itype\""
        echo "}"
    } >> "${spec_file}"

    if [[ "${do_run}" -eq 0 ]]; then
        echo "Confirm the following spec:"
        echo "Machines ${itype} (${inum} machines) on region ${region_name}"
        echo "Specification (file ${spec_file}):"
        jq < "${spec_file}"
        read -p "Are you sure you want to continue? " -n 1 -r
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            echo
        else
            echo -e "\nAborted"
            exit 1
        fi
    fi

    local r

    r=$(aws ec2 request-spot-instances \
        "${set_price}" \
        --type "one-time" \
        --instance-count "${inum}" \
        --query "SpotInstanceRequests[*].[SpotInstanceRequestId]" \
        --launch-specification file://"${spec_file}" \
        --output text \
        --region "${region_name}")

    if [[ $? -ne 0 ]]; then
        echo "Couldn't request instances: ${r}"
        exit 1
    else
        echo "Requested ${inum} ${itype} instances at ${region_name}"
        echo "${r}"
    fi
}

run "$@"
