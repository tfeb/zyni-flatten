#!/bin/sh -

IMPLS="lw sbcl"

ITERS="${1:-100}"
shift

echo "|$(date +%Y%m%d%H%M)|$IMPLS|(bench $ITERS "us" :unit :us $*)|" >>run.log

for impl in $IMPLS
do
    "$impl" <<EOF
(needs ("bench" :compile t))
(bench $ITERS "us" :unit :us $*)
EOF
done
