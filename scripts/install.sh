#!/usr/bin/env bash

sudo apt-get update

# Git, Make, and dig and zlib (for hdr_histogram)
sudo apt-get -y install ntp build-essential git vim dnsutils zlib1g-dev htop numactl
node_name=$(uname -n)
# apollo-2-4 is running Ubuntu 18.04
if [[ "${node_name}" == "apollo-2-4" ]]; then
    deb_name="esl-erlang_23.0.2-2~ubuntu~bionic_amd64.deb"
else
    deb_name="esl-erlang_23.0.2-2~debian~stretch_amd64.deb"
fi

curl -O "https://packages.erlang-solutions.com/erlang/debian/pool/${deb_name}"
# Try once for dependencies, resolve and install again
sudo dpkg -i "${deb_name}"
sudo apt-get -f -y install
sudo dpkg -i "${deb_name}"

