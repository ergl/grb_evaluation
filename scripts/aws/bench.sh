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

do_load_rubis() {
    local confirm_load="${1}"
    local target_machine="${2}"
    local config_file="${3}"

    if [[ "${confirm_load}" -eq 1 ]]; then
        pushd "${HOME}/sources/lasp-bench/scripts"
        ./rubis_load.escript "${target_machine}" "7878" "${config_file}"
        popd
    else
        read -r -n 1 -p "Load target ${target_machine}:7878 ? [y/n] " response
        case "${response}" in
            [yY] )
                pushd "${HOME}/sources/lasp-bench/scripts"
                ./rubis_load.escript "${target_machine}" "7878" "${config_file}"
                popd
                ;;
            *)
                echo -e "\\nLoad aborted"
                ;;
        esac
    fi
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
    echo "bench.sh [-hy] [-b <branch>=bench_grb] dl | compile | run <config> <bootstrap-node> <bootstrap-port> | load_rubis <node> <config> | rebuild | report [config]"
}

run () {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local branch="bench_grb"
    local confirm_load=0
    while getopts ":yb:h" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            b)
                branch="${OPTARG}"
                ;;
            y)
                confirm_load=1
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
        "load_rubis")
            local load_target="${2}"
            local config_file="${3}"
            do_load_rubis "${confirm_load}" "${load_target}" "${config_file}"
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