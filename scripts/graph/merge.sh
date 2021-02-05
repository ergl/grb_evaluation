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
    local aws="${1}"
    local folder="${2}"
    local glob
    if [[ "${aws}" -eq 1 ]]; then
        glob="aws-*"
    else
        glob="apollo-*"
    fi

    # $glob not quoted to allow glob expansion
    # shellcheck disable=SC2086
    ./mergeSummary.awk "${folder}"/${glob}/summary.csv > "${folder}"/summary.csv

    # $glob not quoted to allow glob expansion
    # shellcheck disable=SC2086
    ./mergeErrors.awk "${folder}"/${glob}/errors.csv > "${folder}"/errors.csv

    local token_dir
    # This directory will be used to test if a latency file exists
    # Any directory would do, since in theory they should be all the same
    # Exclude images file, grab the first node file we can find
    token_dir=$(find "${folder}" -type d -not -path "${folder}" -not -path "${folder}/_images" \
                 | head -1 \
                 | xargs basename)

    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "ping_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "uniform-barrier_latencies.csv"

    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "readonly-blue_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "writeonly-blue_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "read-write-blue_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "uniform-barrier_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "readonly-blue-barrier_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "writeonly-blue-barrier_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "read-write-blue-barrier_latencies.csv"

    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "readonly-red_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "writeonly-red_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "read-write-red_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "readonly-red-barrier_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "writeonly-red-barrier_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "read-write-red-barrier_latencies.csv"

    # Rubis operations
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "register-user_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "browse-categories_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "search-items-in-category_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "browse-regions_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "browse-categories-in-region_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "search-items-in-region_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "view-item_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "view-user-info_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "view-bid-history_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "buy-now_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "store-buy-now_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "put-bid_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "store-bid_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "put-comment_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "store-comment_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "select-category-to-sell-item_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "register-item_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "about-me_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "get-auctions-ready-for-close_latencies.csv"
    mergeSiteLatency "${folder}" "${glob}" "${token_dir}" "close-auction_latencies.csv"
}

usage() {
    echo -e "merge.sh [-ah] <input-folder>"
}

run() {
    if [[ $# -lt 1 ]]; then
        usage
        exit 1
    fi

    local aws=0
    while getopts ":ah" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            a)
                aws=1
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

    local input_folder="${1}"
    if [[ ! -d "${input_folder}" ]]; then
        echo "Error: ${input_folder} needs to be a folder"
        exit 1
    fi

    mergeSite "${aws}" "${input_folder}"
    exit $?
}

run "$@"
