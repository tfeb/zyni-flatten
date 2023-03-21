#lang racket/base

(require "plot-times.rkt"
         (only-in racket/file make-directory*))


;;;; Plot a bunch of SVG files for the 20230319 runs
;;;
;;; From https://www.datylon.com/blog/data-visualization-for-colorblind-readers#good-bad-charts
;;; use yellow red blue
;;;
;;; Closing parens on their own line makes it easier to comment output files
;;;

(make-directory* "svg")

(plot-specs '(("data/20230319/treesearch-1000-us-lispworks-arm64.ldat"
                 (0 "implicit stack" "yellow")
                 (1 "vector stack" "blue")))
            #:title "LispWorks treesearch: implicit vs vector stack"
            #:to "svg/lw-treesearch-implicit-vector.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-lispworks-arm64.ldat"
                 (1 "vector stack" "yellow")
                 (4 "consy stack" "blue")))
            #:title "LispWorks treesearch: vector vs consy stack"
            #:to "svg/lw-treesearch-vector-consy.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-lispworks-arm64.ldat"
               (0 "treesearch implicit" "yellow"))
              ("data/20230319/flatten-1000-us-lispworks-arm64.ldat"
               (0 "flatten implicit" "blue")))
            #:title "LispWorks: treesearch vs flatten"
            #:to "svg/lw-treesearch-flatten.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-sbcl-arm64.ldat"
                 (0 "implicit stack" "yellow")
                 (1 "vector stack" "blue")))
            #:title "SBCL treesearch: implicit vs vector stack"
            #:to "svg/sbcl-treesearch-implicit-vector.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-sbcl-arm64.ldat"
                 (1 "vector stack" "yellow")
                 (4 "consy stack" "blue")))
            #:title "SBCL treesearch: vector vs consy stack"
            #:to "svg/sbcl-treesearch-vector-consy.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-sbcl-arm64.ldat"
               (0 "treesearch implicit" "yellow"))
              ("data/20230319/flatten-1000-us-sbcl-arm64.ldat"
               (0 "flatten implicit" "blue")))
            #:title "SBCL: treesearch vs flatten"
            #:to "svg/sbcl-treesearch-flatten.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-lispworks-arm64.ldat"
               (0 "LispWorks implicit" "blue"))
              ("data/20230319/treesearch-1000-us-sbcl-arm64.ldat"
               (0 "SBCL implicit" "yellow")))
            #:title "LispWorks vs SBCL: treesearch implicit stacks"
            #:to "svg/lw-sbcl-treesearch-implicit.svg"
            )

(plot-specs '(("data/20230319/treesearch-1000-us-lispworks-arm64.ldat"
               (0 "LispWorks implicit" "blue"))
              ("data/20230319/treesearch-1000-us-sbcl-arm64.ldat"
               (1 "SBCL vector" "yellow")))
            #:title "LispWorks vs SBCL: treesearch best stacks"
            #:to "svg/lw-sbcl-treesearch-best.svg"
            )

(plot-specs '(("data/20230319/flatten-1000-us-lispworks-arm64.ldat"
               (4 "LispWorks consy" "blue"))
              ("data/20230319/flatten-1000-us-sbcl-arm64.ldat"
               (0 "SBCL consy" "yellow")))
            #:title "LispWorks vs SBCL: flatten consy stacks"
            #:to "svg/lw-sbcl-flatten-consy.svg"
            )
