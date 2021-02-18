#!/usr/bin/env bash

set -euxo pipefail

KIELE_REVISION="$1" ; shift
TEST_PORT="$1"      ; shift

git clean -dffx
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
