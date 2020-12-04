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

kiele vm | awk -F ':' '{print $2}' > port &
sleep 3
export PORT=$(cat port)
make test -j8 -k
make coverage
kill %1
