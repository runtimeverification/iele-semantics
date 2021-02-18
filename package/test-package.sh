#!/usr/bin/env bash

set -euxo pipefail

TEST_PORT="$1" ; shift

make test-vm -j4
make test-iele -j4

kiele vm --port ${TEST_PORT} &
pid=$!
sleep 3
make test-iele-node  -j4 TEST_PORT=${TEST_PORT}
make test-bad-packet -j4 TEST_PORT=${TEST_PORT}
kill $pid
