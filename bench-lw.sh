#!/bin/sh -

LW=lw
ITERS="${1:-100}"
shift

echo "|$(date +%Y%m%d%H%M)|lw|(bench/lw $ITERS "us" :unit :us $*)|" >>run.log

"$LW" <<EOF
(needs ("bench-lw" :compile t))
(bench/lw $ITERS "us" :unit :us $*)
EOF
