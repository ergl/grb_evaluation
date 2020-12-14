#!/usr/bin/env bash

set -eo pipefail

SELF=$(readlink "$0" || true)
if [[ -z ${SELF} ]]; then
  SELF=$0
fi

cd "$(dirname "$SELF")"

graph () {
    local y="${1}"
    local x="${2}"
    local global_step="${3}"
    local single_step="${4}"
    local folder="${5}"

    local image_folder="${folder}/_images"
    mkdir -p "${image_folder}"

    Rscript --vanilla ./grid_summary.r -x "${x}" -y "${y}" -s "${global_step}" -i "${folder}" -o "${image_folder}"/summary.png

    # Even if it contains a /, append one, doesn't change anything
    folder+="/"
    for subfolder in "${folder}"*/; do
        local folder_name=$(basename "${subfolder}")
        # Skip _images folder
        if [[ "${folder_name}" =~ ^_.* ]]; then
            continue
        fi
        Rscript --vanilla ./summary.r -x "${x}" -y "${y}" -s "${single_step}" -i "${subfolder}" -o "${image_folder}/${folder_name}.png"
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
    local width=1500
    local height=2000

    while getopts ":t:s:x:y:h" opt; do
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
            x)
                width="${OPTARG}"
                ;;
            y)
                height="${OPTARG}"
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

    graph "${height}" "${width}" "${total_step}" "${node_step}" "${input_folder}"
    exit $?
}

run "$@"
