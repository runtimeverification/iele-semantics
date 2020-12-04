#!/usr/bin/env bash

set -euxo pipefail

KIELE_VERSION="$1"
UBUNTU_RELEASE="$2"
KIELE_REVISION="$3"

sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes netcat
sudo apt-get install --yes ./kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb

git clone 'https://github.com/runtimeverification/iele-semantics'
cd iele-semantics
git checkout "$KIELE_REVISION"
git submodule update --init --recursive

iele-vm 0 127.0.0.1 > port &
sleep 3
export PORT=$(cat port | awk -F ':' '{print $2}')
make test -j`nproc` -k TEST_WELLFORMED=true
kill %1
