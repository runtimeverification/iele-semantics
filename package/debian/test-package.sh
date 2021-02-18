#!/usr/bin/env bash

set -euxo pipefail

KIELE_VERSION="$1"  ; shift
UBUNTU_RELEASE="$1" ; shift

sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes netcat
sudo apt-get install --yes ./kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb

../test-package.sh "$@"
