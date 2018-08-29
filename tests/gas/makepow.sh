#!/bin/bash
eval echo `echo '(2^'$2')^'$1'/2-1' | bc` | tr -d ' '
