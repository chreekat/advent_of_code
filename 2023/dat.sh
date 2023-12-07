#!/usr/bin/env bash

set -Eeuo pipefail

: ${num:=$(date +%-1d)}

xclip -selection clipboard -o > day${num}.txt
