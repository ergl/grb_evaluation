#!/usr/bin/env bash

set -eo pipefail

DEFAULT_ITYPE="m4.2xlarge"

usage() {
    echo -e "check_settings.sh [region] [instance]=${DEFAULT_ITYPE}"
}

run() {
    if [[ $# -lt 1 ]]; then
        usage
        exit 1
    fi

    local region_name="${1}"
    local instance_type="${2:-${DEFAULT_ITYPE}}"

    echo "Checking type availability"
    r=$(aws ec2 describe-instance-type-offerings \
        --filters "Name=instance-type,Values=${instance_type}" \
        --query "InstanceTypeOfferings[*].InstanceType" \
        --output text \
        --region "${region_name}")

    if [[ "${r}" == "" ]]; then
        echo "Unknown region / type"
        exit 1
    else
        echo "${region_name}: ${r} ok"
    fi

    local image
    echo "Checking image availability"
    image=$(./info.sh "${region_name}" image)
    if [[ $? -eq 1 ]]; then
        echo "No image at ${region_name}"
    else
        echo "${region_name}/image: ${image}"
    fi

    local sg
    echo "Checking security group availability"
    sg=$(./info.sh "${region_name}" sg)
    if [[ $? -eq 1 ]]; then
        echo "No group at ${region_name}"
    else
        echo "${region_name}/sg: ${sg}"
    fi

    local key
    echo "Checking key pair availability"
    key=$(./info.sh "${region_name}" key)
    if [[ $? -eq 1 ]]; then
        echo "No keypair at ${region_name}"
    else
        echo "${region_name}/key: ${key}"
    fi

    echo "Start instances as such:"
    echo "./request_instances.sh -r ${region_name} -t ${instance_type} -i ${image} -k ${key} -s ${sg} -n <amount>"
}

run "$@"
