#!/bin/bash
head -c $1 /dev/urandom | xxd -p | tr -d '\n'
