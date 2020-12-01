#!/usr/bin/env bash

set -euxo pipefail

KIELE_VERSION="$1"
UBUNTU_RELEASE="$2"
KIELE_REVISION="$3"
K_SHORT_REV="$4"

K_RELEASE="https://github.com/kframework/k/releases/download/v5.0.0-${K_SHORT_REV}"

sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes opam netcat
curl --fail --location "${K_RELEASE}/kframework_5.0.0_amd64_${UBUNTU_RELEASE}.deb" --output kframework-${UBUNTU_RELEASE}.deb
sudo apt-get install --yes ./kframework-${UBUNTU_RELEASE}.deb
sudo apt-get install --yes ./kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot k-configure-opam'
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot opam install --yes ocaml-protoc rlp yojson zarith hex uuidm cryptokit'
export OPAMROOT=/usr/lib/kframework/opamroot
eval $(opam config env)

git clone 'https://github.com/runtimeverification/iele-semantics'
cd iele-semantics
git checkout "$KIELE_REVISION"
git submodule update --init --recursive

iele-vm 0 127.0.0.1 > port &
sleep 3
export PORT=$(cat port | awk -F ':' '{print $2}')
make test -j`nproc` -k
make coverage
kill %1
