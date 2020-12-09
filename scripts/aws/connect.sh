#!/usr/bin/env bash

set -eo pipefail

usage() {
    echo "connect.sh region instance_id"
}

run() {
    if [[ $# -ne 2 ]]; then
        usage
        exit 1
    fi

    local region_name="${1}"
    local instance="${2}"
    local ip
    ip=$(aws ec2 describe-instances \
        --instance-ids "${instance}" \
        --query 'Reservations[*].Instances[*].PublicIpAddress' \
        --output text \
        --region "${region_name}")

    if [ "$?" -ne "0" ]; then
        echo "Bad ip: $ip"
        exit 1
    fi
    local key
    key=$(./info.sh "${region_name}" key)
    if [[ $? -eq 1 ]]; then
        echo "No keypair at ${region_name}"
        exit 1
    fi

    ssh -oStrictHostKeyChecking=accept-new -i ./keys/"${key}".pem ubuntu@"${ip}"
}

run "$@"
