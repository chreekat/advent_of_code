#!/usr/bin/env bash

set -Eeuo pipefail

: ${num:=${1:-$(date +%-1d)}}

xclip -selection clipboard -o > day${num}.txt
