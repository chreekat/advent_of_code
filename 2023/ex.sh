#!/usr/bin/env bash

set -Eeuo pipefail

: ${num:=$(date +%-1d)}

numex=$(($(find . -name "day${num}-ex*" | wc -l) + 1))

xclip -selection clipboard -o > day${num}-ex${numex}.txt
