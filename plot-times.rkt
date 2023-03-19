#lang racket

;;;; Some code to plot the ldat files with performance data
;;;
;;; This is all fairly horrible:
;;; - firstly I want to plot grids of data points as functions;
;;; - secondly the format the ldat files are in needs quite a lot of processing
;;;   to turn it into gridded data.
;;;
;;; None of this cares about performance at all.
;;;

(require plot
         srfi/17)

(module+ test
  (require rackunit))

;;;; Grids
;;;

(define (lists->grid listy-grid)
  (let ((rowlen (length (first listy-grid))))
    (for/vector ([row (in-list listy-grid)])
      (unless (= (length row) rowlen)
        (error 'lists->grid "ragged"))
      (list->vector row))))

(define (make-empty-grid rows columns (init 0.0))
  (for/vector ([row (in-range rows)])
    (make-vector columns init)))

(define (grid-dimensions grid)
  (values (vector-length grid)
          (if (> (vector-length grid) 0)
              (vector-length (vector-ref grid 0))
              0)))

(set! (setter vector-ref) vector-set!)

(define gref
  (getter-with-setter
   (λ (grid row column)
     (vector-ref (vector-ref grid row) column))
   (λ (grid row column value)
     (set! (vector-ref (vector-ref grid row) column) value))))

;;;; Making functions from a grid
;;;

(define (make-grid-interpolator grid)
  ;; This is just bilinear interpolation.  x corresponds to the row index,
  ;; y to the column index, so these are just equivalent to i & j but
  ;; continuous.  The way to think of this geometrically is thst the grid has
  ;; been rotated by pi/2 anticlockwise.
  (define-values (max-row max-column)
    (let-values ([(rows columns) (grid-dimensions grid)])
      (values (- rows 1) (- columns 1))))
  (λ (x y)
    (unless (and (<= 0 x max-row)
                 (<= 0 y max-column))
      ;; should use range errors
      (error 'grid "range"))
    (define-values (x0 x1 dx)
      (let* ([fx (floor x)]
             [x0 (inexact->exact fx)])
        (values x0 (add1 x0) (- x fx))))
    (define-values (y0 y1 dy)
      (let* ([fy (floor y)]
             [y0 (inexact->exact fy)])
        (values y0 (add1 y0) (- y fy))))
    (cond
      ;; Deal with all the edge cases (this means plots do not lose edges)
      [(= x0 max-row)
       (if (= y0 max-column)
           (gref grid x0 y0)
           (+ (* (- 1 dy) (gref grid max-row y0))
              (* dy (gref grid max-row y1))))]
      [(= y0 max-column)
       (if (= x0 max-row)
           (gref grid x0 y0)
           (+ (* (- 1 dx) (gref grid x0 max-column))
              (* dx (gref grid x1 max-column))))]
      [else
       (+ (* (- 1 dx) (- 1 dy) (gref grid x0 y0))
          (* dx (- 1 dy) (gref grid x1 y0))
          (* (- 1 dx) dy (gref grid x0 y1))
          (* dx dy (gref grid x1 y1)))])))

(module+ test
  (let* ([gf (make-grid-interpolator
              (lists->grid '((0 0)
                             (1 1))))])
    (check-eqv? (gf 0 0) 0)
    (check-= (gf 0.5 0) 0.5 0)))

;;;; Reading data into grids from ldat files
;;;
;;; Each entry is ((cardepth cdrdepth) vals ...)
;;; cdr should be the x dimension, so ultimately the row index,
;;; car should be y dimension, so ultimately the column index
;;;

(define (snarf from)
  ;; Stolen from warranted (wcs.rkt)
  (call-with-default-reading-parameterization
   (thunk
    (parameterize ([read-accept-lang #f]
                   [read-accept-reader #f])
      (call-with-input-file from read)))))

(define (compute-deltas data)
  ;; compute cardepth and cdrdepth deltas from the data, checking for sanity
  (for/fold ([delta-cardepth 0]
             [delta-cdrdepth 0]
             [previous-cardepth (first (first (first data)))]
             [previous-cdrdepth (second (first (first data)))]
             #:result (values delta-cardepth delta-cdrdepth))
            ([line (in-list data)])
    (match-let ([(list (list cardepth cdrdepth) _ ...) line])
      (values
       (cond
         [(<= cardepth previous-cardepth)
          delta-cardepth]
         [(zero? delta-cardepth)
          (- cardepth previous-cardepth)]
         [(= (- cardepth previous-cardepth) delta-cardepth)
          delta-cardepth]
         [else
          (error 'find-deltas "cardeph mismatch")])
       (cond
         [(<= cdrdepth previous-cdrdepth)
          delta-cdrdepth]
         [(zero? delta-cdrdepth)
          (- cdrdepth previous-cdrdepth)]
         [(= (- cdrdepth previous-cdrdepth) delta-cdrdepth)
          delta-cdrdepth]
         [else
          (error 'find-deltas "cdrdeph mismatch")])
       cardepth
       cdrdepth))))

(define (compute-extrema data)
  ;; Compute the extrema of the car & cdr depths in data
  (for/fold ([cardepth-min #f]
             [cardepth-max #f]
             [cdrdepth-min #f]
             [cdrdepth-max #f])
            ([line (in-list data)])
    (match-let ([(list (list cardepth cdrdepth) _ ...) line])
      (values (if cardepth-min
                  (min cardepth cardepth-min)
                  cardepth)
              (if cardepth-max
                  (max cardepth cardepth-max)
                  cardepth)
              (if cdrdepth-min
                  (min cdrdepth cdrdepth-min)
                  cdrdepth)
              (if cdrdepth-max
                  (max cdrdepth cdrdepth-max)
                  cdrdepth)))))

(define (make-grid-for-data delta-cardepth delta-cdrdepth
                            cardepth-min cardepth-max
                            cdrdepth-min cdrdepth-max)
  ;; make an empty grid for the data
  (define (n dx xmin xmax)
    (let ((maybe-n (+ (/ (- xmax xmin) dx) 1)))
      (unless (integer? maybe-n)
        (error 'make-grid-for-data "botch"))
      maybe-n))
  (make-empty-grid (n delta-cdrdepth cdrdepth-min cdrdepth-max)
                   (n delta-cardepth cardepth-min cardepth-max)))

(define (make-scaler dx xmin xmax #:indexer (indexer #f))
  ;; If scaler is true just scale the result and don't worry about
  ;; range or it being an integer.  Otherwise do.
  (if (not indexer)
      (λ (x) (/ (- x xmin) dx))
      (λ (x)
        (unless (<= xmin x xmax)
          (raise-range-error
           'indexer
           "index"
           "" x "grid" xmin xmax))
        (let ([index (/ (- x xmin) dx)])
          (unless (integer? index)
            (error 'indexer "botch"))
          index))))

(define (data->grids data)
  ;; Return the grids and their deltas & limits
  (define-values (delta-cardepth delta-cdrdepth) (compute-deltas data))
  (define-values (cardepth-min cardepth-max cdrdepth-min cdrdepth-max)
    (compute-extrema data))
  (define grids
    (match-let ([(list (list _ _) vals ...) (first data)])
      (for/list ([v (in-list vals)])
        (make-grid-for-data delta-cardepth delta-cdrdepth
                            cardepth-min cardepth-max
                            cdrdepth-min cdrdepth-max))))
  (define ixcar (make-scaler delta-cardepth cardepth-min cardepth-max
                             #:indexer #t))
  (define ixcdr (make-scaler delta-cdrdepth cdrdepth-min cdrdepth-max
                             #:indexer #t))
  (for ([line (in-list data)])
    (match-let ([(list (list cardepth cdrdepth) vals ...) line])
      (for ([grid (in-list grids)]
            [value (in-list vals)])
        (set! (gref grid (ixcdr cdrdepth) (ixcar cardepth)) value))))
  ;; Return in x y order
  (values grids
          (list delta-cdrdepth cdrdepth-min cdrdepth-max)
          (list delta-cardepth cardepth-min cardepth-max)))

(define (make-scaled-grid-function f dx xmin xmax dy ymin ymax
                                   #:y-scale (y-scale 1))
  ;; Make a suitable function for plotting an interpolated grid function
  (define x-scaler (make-scaler dx xmin xmax))
  (define y-scaler (make-scaler dy ymin ymax))
  (λ (x y) (* y-scale (f (x-scaler x) (y-scaler y)))))

(define sample-data '(((0 1) 1.0)
                      ((0 2) 2.0)
                      ((0 3) 3.0)
                      ((1 1) 1.5)
                      ((1 2) 2.5)
                      ((1 3) 3.5)))

(define (data->interpolators data)
  ;; Return a list of interpolators and the parameters
  (let-values ([(grids xs ys) (data->grids data)])
    (match-let ([(list dx xmin xmax) xs]
                [(list dy ymin ymax) ys])
      (values (map (λ (grid)
                     (make-scaled-grid-function
                      (make-grid-interpolator grid)
                      dx xmin xmax
                      dy ymin ymax))
                   grids)
              xs ys))))

;;;; Plotting interpolators
;;;

;;; Default params
(plot-font-family 'modern)
(plot-width 560)

(define (plotter-for-interpolator interpolator xs ys)
  (match-let ([(list _ xmin xmax) xs]
              [(list _ ymin ymax) ys])
    (surface3d interpolator xmin xmax ymin ymax)))
