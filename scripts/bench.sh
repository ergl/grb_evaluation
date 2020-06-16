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
    pushd "${HOME}/sources/lasp-bench"
    make
    popd
}

do_load() {
    local confirm_load="${1}"
    local target_machine="${2}"

    pushd "${HOME}/sources/lasp-bench/scripts"
    if [[ "${confirm_load}" -eq 1 ]]; then
        ./grb_load.escript "${target_machine}" "7878"
    else
        read -r -n 1 -p "Load target ${target_machine}:7878 ? [y/n] " response
        case "${response}" in
            [yY] )
                pushd "${HOME}/sources/lasp-bench/scripts"
                ./grb_load.escript "${target_machine}" "7878"
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
    pushd "${HOME}/sources/lasp-bench"
    ./_build/default/bin/lasp_bench "${config}"
    popd
}

do_tc() {
    local dry_run="${1}"
    local own_cluster="${2}"
    local cluster_config="${3}"

    pushd "${HOME}/sources/lasp-bench/scripts"
    if [[ "${dry_run}" -eq 1 ]]; then
        escript -c -n ./build_tc_rules.escript -d -c "${own_cluster}" -f "${cluster_config}"
    else
        escript -c -n ./build_tc_rules.escript -c "${own_cluster}" -f "${cluster_config}"
    fi
    popd
}

do_tclean() {
    # apollo-2-4 runs Ubuntu, default iface is eth0
    local node_name=$(uname -n)
    if [[ "${node_name}" == "apollo-2-4" ]]; then
        sudo tc qdisc del dev eth0 root
    else
        sudo tc qdisc del dev enp1s0 root
    fi
}

usage() {
    echo "bench.sh [-hdy] [-b <branch>=bench_grb] dl | compile | load [machine] | tc [cluster] [config] | tclean | run [config] | rebuild | report [config]"
}

run () {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local branch="bench_grb"
    local tc_dry_run=0
    local confirm_load=0
    while getopts ":dyb:h" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            b)
                branch="${OPTARG}"
                ;;
            d)
                tc_dry_run=1
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
        "load")
            local default_target
            default_target=$(get_default_load_target)
            local load_target="${2:-${default_target}}"
            do_load "${confirm_load}" "${load_target}"
            ;;
        "tc")
            if [[ "${branch}" != "bench_grb" ]]; then
                echo "tc script is only available on bench_grb branch"
                exit 1
            fi
            local cluster_name="${2}"
            local tc_config="${3}"
            do_tc "${tc_dry_run}" "${cluster_name}" "${tc_config}"
            ;;
        "tclean")
            if [[ "${branch}" != "bench_grb" ]]; then
                echo "tc script is only available on bench_grb branch"
                exit 1
            fi
            do_tclean
            exit $?
            ;;
        "run")
            local run_config_file="${2:-${HOME}/sources/lasp-bench/examples/grb.config}"
            echo -e "Runnig with ${run_config_file}\n"
            do_run "${run_config_file}"
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
