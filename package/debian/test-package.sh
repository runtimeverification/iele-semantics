#!/usr/bin/env bash

set -euxo pipefail

KIELE_VERSION="$1"
UBUNTU_RELEASE="$2"

sudo apt-get update && sudo apt-get upgrade --yes
sudo apt-get install --yes ./kiele_${KIELE_VERSION}_amd64_${UBUNTU_RELEASE}.deb
