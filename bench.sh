#!/bin/sh -

IMPLS="lw sbcl"

ITERS="${1:-100}"
shift

for impl in $IMPLS
do
    "$impl" <<EOF
(needs ("bench" :compile t))
(bench $ITERS "us" :unit :us $*)
EOF
done
