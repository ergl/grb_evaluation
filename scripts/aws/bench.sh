#!/usr/bin/env bash

set -eo pipefail

REPO_URL="https://github.com/ergl/lasp-bench.git"

do_download() {
    local branch="${1}"
    local folder="${2}"
    git clone "${REPO_URL}" --single-branch --branch "${branch}" "${folder}"
}

do_compile() {
    pushd "${HOME}/sources/lasp-bench"
    make
    popd
}

do_rebuild() {
    local branch="${1}"
    pushd "${HOME}/sources/lasp-bench"
    git fetch origin
    git reset --hard origin/"${branch}"
    popd
}

do_run() {
    local config="${1}"
    local node="${2}"
    local port="${3}"
    pushd "${HOME}/sources/lasp-bench"
    (
        export BOOTSTRAP_NODE="${node}"; export BOOTSTRAP_PORT="${port}"; ./_build/default/bin/lasp_bench "${config}"
    )
    popd
}

usage() {
    echo "bench.sh [-h] [-b <branch>=bench_grb] dl | compile | run <config> <bootstrap-node> <bootstrap-port> | rebuild | report [config]"
}

run () {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local branch="bench_grb"
    while getopts ":b:h" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            b)
                branch="${OPTARG}"
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

    if [[ $# -lt 1 ]]; then
        usage
        exit 1
    fi

    local command="${1}"
    case $command in
        "dl")
            do_download "${branch}" "${HOME}/sources/lasp-bench"
            ;;
        "compile")
            do_compile
            exit $?
            ;;
        "run")
            local run_config_file="${2}"
            local bootstrap_node="${3}"
            local bootstrap_port="${4:-7878}"
            echo -e "Running with ${run_config_file}\n"
            do_run "${run_config_file}" "${bootstrap_node}" "${bootstrap_port}"
            exit $?
            ;;
        "rebuild")
            do_rebuild "${branch}"
            do_compile
            exit $?
            ;;
        *)
            echo "Unrecognized command ${command}"
            usage
            exit 1
            ;;
    esac
}

run "$@"
