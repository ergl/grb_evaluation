#!/usr/bin/env bash

set -eo pipefail

run() {
    local node_name="${1:-"apollo-1-1.imdea"}"
    local node_ip=$(dig +short "${node_name}")
    erl -name health@127.0.0.1 -setcookie grb_cookie -eval "net_kernel:connect_node('grb@${node_ip}')" -run observer
}

run "$@"
