#!/usr/bin/env bash

set -eo pipefail

SELF=$(readlink "$0" || true)
if [[ -z ${SELF} ]]; then
  SELF=$0
fi

cd "$(dirname "$SELF")"

mergeSiteLatency() {
    local folder="${1}"
    local glob="${2}"
    local token_dir="${3}"
    local csv_file="${4}"

    if [[ -f "${folder}"/"${token_dir}"/"${csv_file}" ]]; then
        # $glob not quoted to allow glob expansion
        # shellcheck disable=SC2086
        ./mergeLatencies.awk "${folder}"/${glob}/"${csv_file}" > "${folder}"/"${csv_file}"
    fi
}

mergeSite() {
    local apollo="${1}"
    local folder="${2}"

    local glob
    if [[ "${apollo}" -eq 1 ]]; then
        glob="apollo-*"
    else
        glob="bench-*"
    fi

    # $glob not quoted to allow glob expansion
    # shellcheck disable=SC2086
    ./mergeSummary.awk "${folder}"/${glob}/summary.csv > "${folder}"/summary.csv

    local token_dir
    # This directory will be used to test if a latency file exists
    # Any directory would do, since in theory they should be all the same
    # Exclude images file, grab the first node file we can find
    token_dir=$(find "${folder}" -type d -not -path "${folder}" -not -path "${folder}/_images" \
                 | head -1 \
                 | xargs basename)

    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "ping_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "readonly-blue_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "writeonly-blue_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "read-write-blue_latencies.csv"
}

usage() {
    echo -e "merge.sh [-hd] input-folder
    -a\t\tMerge nodes from apollo
    -h\t\tShows this help"
}

run() {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local apollo=0
    while getopts ":ah" opt; do
        case $opt in
            a)
                apollo=1
                ;;
            h)
                usage
                exit 0
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

    mergeSite "${apollo}" "${input_folder}"
    exit $?
}

run "$@"
