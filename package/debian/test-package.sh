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

make test-vm -j4
make test-iele -j4
make test-interactive

kiele vm --port 9001 &
pid=$!
sleep 3
make test-iele-node  -j4 TEST_PORT=9001
make test-bad-packet -j4 TEST_PORT=9001
kill $pid
