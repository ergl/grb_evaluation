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

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"

    IP="${node_ip}" RIAK_RING_SIZE="${ring_size}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env start
    sleep 2
    IP="${node_ip}" RIAK_RING_SIZE="${ring_size}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env ping

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

do_restart() {
    local node_ip
    local folder="${1}"
    local ring_size="${2}"

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"

    IP="${node_ip}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env stop
    rm -rf _build/"${REBAR_PROFILE}"/rel
    ./rebar3 as "${REBAR_PROFILE}" release -n "${APP_NAME}"
    IP="${node_ip}" RIAK_RING_SIZE="${ring_size}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env start
    sleep 2
    IP="${node_ip}" RIAK_RING_SIZE="${ring_size}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env ping

    popd
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

    node_ip=$(get_ip)

    pushd "${HOME}/sources/${folder}"
    IP="${node_ip}" RIAK_RING_SIZE="${ring_size}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env start
    sleep 2
    IP="${node_ip}" RIAK_RING_SIZE="${ring_size}" ./_build/"${REBAR_PROFILE}"/rel/"${APP_NAME}"/bin/env ping
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
    echo -e "server.sh [-h] [-d] [-r <ring_number>=64] [-b <branch>=master] <command>
Commands:
dl <folder>=branch\tDownloads ${APP_NAME} with the selected branch to the given folder.
\t\t\tDefault is the given branch name in the local folder.
compile
join <config> \tJoins the nodes listed in the config file
start
stop
restart \tReboots ${APP_NAME}, cleaning the release
rebuild \tRecompiles ${APP_NAME} from a fresh copy of the repo
"
}

run() {
    if [[ $# -eq 0 ]]; then
        usage
        exit 1
    fi

    local branch="master"
    local ring_number="64"
    while getopts ":b:dr:h" opt; do
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
            do_run "${branch}" "${ring_number}"
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

        "restart")
            do_restart "${branch}" "${ring_number}"
            exit $?
            ;;

        "start")
            do_start "${branch}" "${ring_number}"
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
