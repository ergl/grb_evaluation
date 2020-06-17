#!/usr/bin/env bash

set -eo pipefail

SELF=$(readlink "$0" || true)
if [[ -z ${SELF} ]]; then
  SELF=$0
fi

cd "$(dirname "$SELF")"

pull () {
    local name="${1}"
    local type="${2}"
    case "${name}" in
        "blotter")
            scp -i ~/.ssh/imdea_id_rsa bderegil@access.grid5000.fr:nancy/*.tar ../blotter/"${type}"
            ;;
        "blotter-cc")
            scp -i ~/.ssh/imdea_id_rsa bderegil@access.grid5000.fr:nancy/*.tar ../master-tcp/"${type}"
            ;;
        "rubis")
            scp -i ~/.ssh/imdea_id_rsa bderegil@access.grid5000.fr:nancy/*.tar ../rubis/
            ;;
    esac
}

run () {
    if [[ $# -ne 2 ]]; then
        echo "pull.sh benchmark-name benchmark-type"
    fi

    local benchmark_name="${1}"
    local benchmark_type="${2}"
    pull "${benchmark_name}" "${benchmark_type}"
}

run "$@"
