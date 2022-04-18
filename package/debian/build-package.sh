#!/usr/bin/env bash

set -euxo pipefail

K_RELEASE="$1"
UBUNTU_RELEASE="$2"

ubuntu_release_descriptor=''
case ${UBUNTU_RELEASE} in
    bionic) ubuntu_release_descriptor='Bionic (18.04)' ;;
    focal)  ubuntu_release_descriptor='Focal (20.04)'  ;;
esac

K_RELEASE_URL=$(curl -sL 'https://api.github.com/repos/kframework/k/releases' | jq --raw-output '. | map(select(.tag_name == "'${K_RELEASE}'")) | map(.assets)[0] | map(select(.label == "Ubuntu '"${ubuntu_release_descriptor}"' Package"))[0] | .browser_download_url')
curl --fail --location "${K_RELEASE_URL}" --output kframework-${UBUNTU_RELEASE}.deb
sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes ./kframework-${UBUNTU_RELEASE}.deb
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot k-configure-opam'
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot opam install --yes ocaml-protoc rlp yojson zarith hex uuidm cryptokit'
export OPAMROOT=/usr/lib/kframework/opamroot
cp -r package/debian ./
mv debian/control.${UBUNTU_RELEASE} debian/control
dpkg-buildpackage
mv ../kiele_${KIELE_VERSION}_amd64.deb ../kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb
