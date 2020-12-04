#!/usr/bin/env bash

set -euxo pipefail

K_SHORT_REV="$1"
UBUNTU_RELEASE="$2"

K_RELEASE="https://github.com/kframework/k/releases/download/v5.0.0-${K_SHORT_REV}"
curl --fail --location "${K_RELEASE}/kframework_5.0.0_amd64_${UBUNTU_RELEASE}.deb" --output kframework-${UBUNTU_RELEASE}.deb
sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes ./kframework-${UBUNTU_RELEASE}.deb
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot k-configure-opam'
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot opam install --yes ocaml-protoc rlp yojson zarith hex uuidm cryptokit'
export OPAMROOT=/usr/lib/kframework/opamroot
cp -r package/debian ./
dpkg-buildpackage
mv ../kiele_${KIELE_VERSION}_amd64.deb ../kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb
