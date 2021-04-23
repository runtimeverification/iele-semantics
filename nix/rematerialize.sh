#!/bin/sh

$(nix-build --no-out-link iele-assemble -A rematerialize)
