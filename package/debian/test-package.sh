#!/usr/bin/env bash

set -euxo pipefail

KIELE_VERSION="$1"
UBUNTU_RELEASE="$2"
KIELE_REVISION="$3"
TEST_PORT="$4"

sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes netcat
sudo apt-get install --yes ./kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb

git clone 'https://github.com/runtimeverification/iele-semantics'
cd iele-semantics
git checkout "$KIELE_REVISION"
git submodule update --init --recursive

make test-vm -j4
make test-iele -j4

kiele vm --port ${TEST_PORT} &
pid=$!
sleep 3
make test-iele-node  -j4 TEST_PORT=${TEST_PORT}
make test-bad-packet -j4 TEST_PORT=${TEST_PORT}
kill $pid
