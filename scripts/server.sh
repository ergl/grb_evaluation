#!/usr/bin/env bash

set -eo pipefail

APP_NAME="grb"
REBAR_PROFILE="default"
REPO_URL="https://github.com/ergl/grb.git"

get_ip() {
    local node_name
    node_name=$(uname -n | sed 's/$/.imdea/' | xargs dig +short)
    echo "${node_name}"
}

do_download() {
    local branch="${1}"
    local folder="${2}"
    git clone "${REPO_URL}" --single-branch --branch "${branch}" "${folder}"
}

do_compile() {
    local folder="${1}"
    pushd "${HOME}/sources/${folder}"

    ./rebar3 as "${REBAR_PROFILE}" compile
    ./rebar3 as "${REBAR_PROFILE}" release -n "${APP_NAME}"

    popd
}

do_run() {
    local node_ip
    local folder="${1}"
    local ring_size="${2}"
    local repl_int="${3}"
    local br_int="${4}"
    local pr_int="${5}"

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"

    IP="${node_ip}" \
    RIAK_RING_SIZE="${ring_size}" \
    REPLICATION_INTERVAL_MS="${repl_int}" \
    BCAST_KNOWN_VC_INTERVAL_MS="${br_int}" \
    COMMITTED_BLUE_PRUNE_INTERVAL_MS="${pr_int}" \
    ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env start

    sleep 2

    IP="${node_ip}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env ping

    popd
}

# User supplies a config file of erlang terms, join script will parse it and
# generate node names from it
do_join() {
    local folder="${1}"
    local config_file="${2}"

    pushd "${HOME}/sources/${folder}"
    ./bin/join_cluster_script.erl -f "${config_file}"
    popd
}

do_connect() {
    local folder="${1}"
    local config_file="${2}"

    pushd "${HOME}/sources/${folder}"
    ./bin/connect_dcs.erl -f "${config_file}"
    popd
}

do_restart() {
    local node_ip
    local folder="${1}"
    local ring_size="${2}"
    local repl_int="${3}"
    local br_int="${4}"
    local pr_int="${5}"

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"

    IP="${node_ip}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env stop
    rm -rf _build/"${REBAR_PROFILE}"/rel
    ./rebar3 as "${REBAR_PROFILE}" release -n "${APP_NAME}"

    IP="${node_ip}" \
    RIAK_RING_SIZE="${ring_size}" \
    REPLICATION_INTERVAL_MS="${repl_int}" \
    BCAST_KNOWN_VC_INTERVAL_MS="${br_int}" \
    COMMITTED_BLUE_PRUNE_INTERVAL_MS="${pr_int}" \
    ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env start

    sleep 2

    IP="${node_ip}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env ping

    popd
}

do_tc() {
    local folder="${1}"
    local own_cluster="${2}"
    local cluster_config="${3}"

    pushd "${HOME}/sources/${folder}"

    escript -c -n ./bin/build_tc_rules.escript -c "${own_cluster}" -f "${cluster_config}"

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

do_rebuild() {
    local node_ip
    local branch="${1}"
    local folder="${2}"

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"
    rm -rf _build/"${REBAR_PROFILE}"/

    git fetch origin
    git reset --hard origin/"${branch}"

    ./rebar3 as "${REBAR_PROFILE}" compile
    ./rebar3 as "${REBAR_PROFILE}" release -n "${APP_NAME}"
    popd
}

do_start() {
    local node_ip
    local folder="${1}"
    local ring_size="${2}"
    local repl_int="${3}"
    local br_int="${4}"
    local pr_int="${5}"

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"

    IP="${node_ip}" \
    RIAK_RING_SIZE="${ring_size}" \
    REPLICATION_INTERVAL_MS="${repl_int}" \
    BCAST_KNOWN_VC_INTERVAL_MS="${br_int}" \
    COMMITTED_BLUE_PRUNE_INTERVAL_MS="${pr_int}" \
    ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env start

    sleep 2

    IP="${node_ip}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env ping
    popd
}

do_stop() {
    local node_ip
    local folder="${1}"

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"
    IP="${node_ip}" ./_build/"${REBAR_PROFILE}"/rel/${APP_NAME}/bin/env stop
    popd
}

usage() {
    cat <<- EOF
server.sh [-hdrbplu] <command>

Flags
    -h                                      Display this help
    -d                                      Dry run
    -r [ring_number]=64                     Number of partitions
    -b [branch_name]=master                 Target git branch to use
    -p [prune_interval]=50                  Ms between committedBlue pruning
    -l [local_broadcast_interval]=5         Ms between local knownVC broadcast
    -u [uniform_replication_interval]=5     Ms between replication/uniformVC computation

Commands
    dl <folder>=branch                      Downloads ${APP_NAME} with the selected branch to the given folder.
                                            Default is the given branch name in the local folder.
    compile
    join <config>                           Joins the nodes listed in the config file
    connect_dcs <config>                    Connects all replicas listed in the config file
    tc [cluster] [config]                   Creates the netem rules to other clusters
    tclean                                  Clean up the netem rules
    start
    stop
    restart                                 Reboots ${APP_NAME}, cleaning the release
    rebuild                                 Recompiles ${APP_NAME} from a fresh copy of the repo
EOF
}

run() {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local branch="master"
    local ring_number=64
    local prune_int=50
    local repl_int=5
    local broadcast_int=5
    while getopts ":r:b:p:l:yu:dh" opt; do
        case $opt in
            h)
                usage
                exit 0
                ;;
            b)
                branch="${OPTARG}"
                ;;
            r)
                ring_number="${OPTARG}"
                ;;
            p)
                prune_int="${OPTARG}"
                ;;
            l)
                broadcast_int="${OPTARG}"
                ;;
            u)
                repl_int="${OPTARG}"
                ;;
            d)
                REBAR_PROFILE="debug_log"
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
            local dl_target="${2:-"sources/${branch}"}"
            do_download "${branch}" "${dl_target}"
            exit $?
            ;;

        "compile")
            do_compile "${branch}"
            exit $?
            ;;

        "run")
            do_run "${branch}" "${ring_number}" "${repl_int}" "${broadcast_int}" "${prune_int}"
            exit $?
            ;;

        "join")
            if [[ $# -lt 2 ]]; then
                usage
                exit 1
            fi

            local node_file_path="${2}"
            do_join "${branch}" "${node_file_path}"
            exit $?
            ;;

        "connect_dcs")
            if [[ $# -lt 2 ]]; then
                usage
                exit 1
            fi

            local node_file_path="${2}"
            do_connect "${branch}" "${node_file_path}"
            exit $?
            ;;

        "tc")
            local cluster_name="${2}"
            local tc_config="${3}"
            do_tc "${branch}" "${cluster_name}" "${tc_config}"
            ;;

        "tclean")
            do_tclean
            exit $?
            ;;

        "restart")
            do_restart "${branch}" "${ring_number}" "${repl_int}" "${broadcast_int}" "${prune_int}"
            exit $?
            ;;

        "start")
            do_start "${branch}" "${ring_number}" "${repl_int}" "${broadcast_int}" "${prune_int}"
            exit $?
            ;;

        "stop")
            do_stop "${branch}"
            exit $?
            ;;

        "rebuild")
            do_rebuild "${branch}" "${branch}"
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
