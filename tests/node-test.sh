#!/bin/bash

kiele vm --port 9001 &
sleep 3
make test-iele-node  "$@" TEST_PORT=9001
make test-bad-packet "$@" TEST_PORT=9001
kill %kiele
