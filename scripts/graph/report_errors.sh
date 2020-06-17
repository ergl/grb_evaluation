#!/usr/bin/env bash

set -eo pipefail

SELF=$(readlink "$0" || true)
if [[ -z ${SELF} ]]; then
  SELF=$0
fi

cd "$(dirname "$SELF")"

report_errors() {
    local folder="${1}"
    local glob="apollo-*"

    local total_errors
    local read_aborts
    local read_abort_rate
    local ronly_read_aborts
    local ronly_read_abort_rate
    local rwrite_read_aborts
    local rwrite_read_abort_rate
    local commit_aborts
    local commit_abort_rate

    total_errors=$(cut -d, -f5 "${folder}"/summary.csv\
                    | grep -v failed\
                    | grep -v "^$"\
                    | paste -sd+ -\
                    | bc)

    # don't quote glob so that it expands
    # shellcheck disable=SC2086
    read_aborts=$(grep "report_total_errors.*maxvc_bad_vc" "${folder}"/${glob}/console.log\
                    | awk '{print $NF}'\
                    | paste -sd+ -\
                    | bc || echo 0)

    # shellcheck disable=SC2086
    ronly_read_aborts=$(grep "report_total_errors.*readonly.*maxvc_bad_vc" "${folder}"/${glob}/console.log\
                        | awk '{print $NF}'\
                        | paste -sd+ -\
                        | bc || echo 0)

    # shellcheck disable=SC2086
    rwrite_read_aborts=$(grep "report_total_errors.*readwrite.*maxvc_bad_vc" "${folder}"/${glob}/console.log\
                    | awk '{print $NF}'\
                    | paste -sd+ -\
                    | bc || echo 0)

    read_abort_rate=$(bc -l <<< "${read_aborts}"/"${total_errors}")
    ronly_read_abort_rate=$(bc -l <<< "${ronly_read_aborts}"/"${total_errors}")
    rwrite_read_abort_rate=$(bc -l <<< "${rwrite_read_aborts}"/"${total_errors}")
    commit_aborts=$((total_errors - read_aborts))
    commit_abort_rate=$(bc -l <<< "${commit_aborts}"/"${total_errors}")

    LC_NUMERIC=en_US printf "Total errors: %'.f; of which read aborts %'.f and %'.f commit aborts (%'.f read aborts in readonly, %'.f read aborts in readwrite)\n"\
                            "${total_errors}" "${read_aborts}" "${commit_aborts}" "${ronly_read_aborts}" "${rwrite_read_aborts}"

    echo "Total abort rates (during reads | during commit):"
    LC_NUMERIC=en_US printf "|%'.6f|%'.6f|\n" "${read_abort_rate}" "${commit_abort_rate}"
    echo "Read abort rates (in read-only tx | in update tx):"
    LC_NUMERIC=en_US printf "|%'.6f|%'.6f|\n" "${ronly_read_abort_rate}" "${rwrite_read_abort_rate}"
}

usage() {
    echo -e "report_errors.sh input-folder"
}

run() {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local input_folder="${1}"
    if [[ ! -d "${input_folder}" ]]; then
        echo "Error: ${input_folder} needs to be a folder"
        exit 1
    fi

    report_errors "${input_folder}"
    exit $?
}

run "$@"
