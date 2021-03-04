#!/bin/bash

port=9001

args=()
while [[ $# -gt 0 ]]; do
    arg="$1"
    case $arg in
        --port) port="$2"    ; shift 2 ;;
        *)      args+=("$1") ; shift   ;;
    esac
done
if [[ "${#args[@]}" -gt 0 ]]; then
  set -- "${args[@]}"
fi

kiele vm --port "$port" &
sleep 3
make test-iele-node  "$@" TEST_PORT="$port"
make test-bad-packet "$@" TEST_PORT="$port"
kill %kiele
