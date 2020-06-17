#!/usr/bin/env bash

set -eo pipefail

SELF=$(readlink "$0" || true)
if [[ -z ${SELF} ]]; then
  SELF=$0
fi

cd "$(dirname "$SELF")"

graph () {
    local global_step="${1}"
    local single_step="${2}"
    local folder="${3}"

    local image_folder="${folder}/_images"
    mkdir -p "${image_folder}"

    Rscript --vanilla ./grid_summary.r -s "${global_step}" -i "${folder}" -o "${image_folder}"/summary.png

    # Even if it contains a /, append one, doesn't change anything
    folder+="/"
    for subfolder in "${folder}"*/; do
        local folder_name=$(basename "${subfolder}")
        # Skip _images folder
        if [[ "${folder_name}" =~ ^_.* ]]; then
            continue
        fi
        Rscript --vanilla ./summary.r -s "${single_step}" -i "${subfolder}" -o "${image_folder}/${folder_name}.png"
    done
}

usage() {
    echo "do_graph.sh [-t <total-step>] [-s <node-step>] [-h] input-folder"
}

run () {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local total_step=10000
    local node_step=5000
    local input_folder

    while getopts ":t:s:h" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            t)
                total_step="${OPTARG}"
                ;;
            s)
                node_step="${OPTARG}"
                ;;
            :)
                echo "Option -${OPTARG} requires an argument"
                usage
                exit 1
                ;;
            *)
                usage
                exit 1
                ;;
        esac
    done

    shift $((OPTIND - 1))

    if [[ $# -ne 1 ]]; then
        usage
        exit 1
    fi

    local input_folder="${1}"
    if [[ ! -d "${input_folder}" ]]; then
        echo "Error: ${input_folder} needs to be a folder"
        exit 1
    fi

    graph "${total_step}" "${node_step}" "${input_folder}"
    exit $?
}

run "$@"
