#!/usr/bin/env bash

set -eo pipefail

REPO_URL="https://github.com/ergl/lasp-bench.git"

get_default_load_target() {
    local node_ip;
    node_ip=$(dig +short "apollo-1-1.imdea")
    echo "${node_ip}"
}

do_download() {
    local branch="${1}"
    local folder="${2}"
    git clone "${REPO_URL}" --single-branch --branch "${branch}" "${folder}"
}

do_compile() {
    local profile="${1}"
    pushd "${HOME}/sources/lasp-bench"
    ./rebar3 as "${profile}" compile
    ./rebar3 as "${profile}" escriptize
    popd
}

do_load_grb() {
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
                ./grb_load.escript "${target_machine}" "7878" "${config_file}"
                popd
                ;;
            *)
                echo -e "\\nLoad aborted"
                ;;
        esac
    fi
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
    local profile="${1}"
    local config="${2}"
    local node="${3}"
    local port="${4}"
    pushd "${HOME}/sources/lasp-bench"
    (
        export BOOTSTRAP_NODE="${node}"; export BOOTSTRAP_PORT="${port}"; ./_build/"${profile}"/bin/lasp_bench "${config}"
    )
    popd
}

usage() {
    echo "bench.sh [-hy] [-b <branch>=bench_grb] [-p <profile>=default] dl | compile | load_grb <node> <config> | load_rubis <node> <config> | run <config> <bootstrap-node> <bootstrap-port> | rebuild"
}

run () {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local branch="bench_grb"
    local profile="default"
    local confirm_load=0
    while getopts ":yb:p:h" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            b)
                branch="${OPTARG}"
                ;;
            p)
                profile="${OPTARG}"
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
            do_compile "${profile}"
            exit $?
            ;;
        "load_grb")
            local default_target
            default_target=$(get_default_load_target)
            local load_target="${2:-${default_target}}"
            local config_file="${3}"
            do_load_grb "${confirm_load}" "${load_target}" "${config_file}"
            ;;
        "load_rubis")
            local default_target
            default_target=$(get_default_load_target)
            local load_target="${2:-${default_target}}"
            local config_file="${3}"
            do_load_rubis "${confirm_load}" "${load_target}" "${config_file}"
            ;;
        "run")
            local run_config_file="${2}"
            local bootstrap_node="${3}"
            local bootstrap_port="${4:-7878}"
            echo -e "Runnig with ${run_config_file}\n"
            do_run "${profile}" "${run_config_file}" "${bootstrap_node}" "${bootstrap_port}"
            exit $?
            ;;
        "rebuild")
            do_rebuild "${branch}"
            do_compile "${profile}"
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
