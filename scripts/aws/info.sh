#!/usr/bin/env bash

set -eo pipefail

usage() {
    echo -e "info.sh [region] [command]"
    echo -e "  where commands are:"
    echo -e "  key"
    echo -e "  image"
    echo -e "  sg"
    echo -e "  help"
}

run() {
    if [[ $# -ne 2 ]]; then
        usage
        exit 1
    fi

    local image_name="erlang_23"
    local sg_prefix="bregil_imdea_sg_"

    local region_name="${1}"
    local command="${2}"
    case $command in
        "key")
            local kp=$(aws ec2 describe-key-pairs \
                --query "KeyPairs[*].KeyName" \
                --output text \
                --region "${region_name}")
            if [[ "${kp}" == "" ]]; then
                echo "Unknown image_name / region, or no key pair found"
                echo "Visit https://docs.aws.amazon.com/cli/latest/userguide/cli-services-ec2-keypairs.html"
                exit 1
            else
                echo "${kp}"
            fi
            ;;
        "image")
            local id=$(aws ec2 describe-images \
                --filter "Name=name,Values=${image_name}" \
                --query 'Images[*].ImageId' \
                --output text \
                --region "${region_name}")
            if [[ "${id}" == "" ]]; then
                echo "Unknown image_name / region, or no image found"
                echo "Visit https://awscli.amazonaws.com/v2/documentation/api/latest/reference/ec2/create-image.html"
                exit 1
            else
                echo "${id}"
            fi
            ;;
        "sg")
            local sg=$(aws ec2 describe-security-groups \
                --filter "Name=group-name,Values=${sg_prefix}${region_name}" \
                --query 'SecurityGroups[*].GroupId' \
                --output text \
                --region "${region_name}")
            if [[ "${sg}" == "" ]]; then
                echo "Unknown image_name / region, or no security group found"
                echo "Visit https://docs.aws.amazon.com/cli/latest/userguide/cli-services-ec2-sg.html"
                exit 1
            else
                echo "${sg}"
            fi
            ;;
        *)
            echo "Unrecognized command ${command}"
            usage
            exit 1
            ;;
    esac
}

run "$@"
