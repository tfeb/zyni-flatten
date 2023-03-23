#lang racket/base

(require "plot-times.rkt"
         (only-in racket/file make-directory*))


;;;; Plot a SVG file for the 20230323 run
;;;
;;; From https://www.datylon.com/blog/data-visualization-for-colorblind-readers#good-bad-charts
;;; use yellow red blue
;;;
;;; Closing parens on their own line makes it easier to comment output files
;;;

(make-directory* "svg")

(plot-specs '(("data/20230323/lw-treesearch-100-us-lispworks-arm64.ldat"
                 (0 "implicit stack" "yellow")
                 (1 "vector stack" "blue")))
            #:title "LispWorks treesearch: implicit vs vector stack, deep tree"
            #:to "svg/lw-treesearch-implicit-vector-deep.svg"
            )
