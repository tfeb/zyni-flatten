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

(define (grid-interpolator grid)
  ;; This is just bilinear interpolation.  x is column, y is row
  (define-values (max-row max-column)
    (let-values ([(rows columns) (grid-dimensions grid)])
      (values (- rows 1) (- columns 1))))
  (λ (x y)
    (unless (<= 0 x max-column)
      (raise-range-error
       'grid
       "grid"
       "" x grid 0 max-column))
    (unless (<= 0 y max-row)
      (raise-range-error
       'grid
       "grid"
       "" y grid 0 max-row))
    (define-values (x0 x1 dx)
      (let ([xi (inexact->exact x)])
        (values (floor xi) (ceiling xi)
                (- x (floor x)))))
    (define-values (y0 y1 dy)
      (let ([yi (inexact->exact y)])
        (values (floor yi) (ceiling yi)
                (- y (floor y)))))
    (+ (* (- 1 dx) (- 1 dy) (gref grid y0 x0))
       (* dx (- 1 dy) (gref grid y0 x1))
       (* (- 1 dx) dy (gref grid y1 x0))
       (* dx dy (gref grid y1 x1)))))

(module+ test
  (let* ([g (lists->grid '((0 1)
                           (2 3)))]
         [gf (grid-interpolator g)])
    (let-values ([(rows columns) (grid-dimensions g)])
      (check-eqv? rows 2)
      (check-eqv? columns 2))
    (check-eqv? (gf 0 0) 0)
    (check-eqv? (gf 0 1) 2)))

;;;; Reading data into grids from ldat files
;;;
;;; Each entry is ((cardepth cdrdepth) implicit explicit adjust consy)
;;; car will be y, which will be row, cdr will be x which will be column
;;; The access order is just going to be fucked up

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
  (for/fold ([cardepth-min 0]
             [cardepth-max 0]
             [cdrdepth-min 0]
             [cdrdepth-max 0])
            ([line (in-list data)])
    (match-let ([(list (list cardepth cdrdepth) _ ...) line])
      (values (min cardepth cardepth-min)
              (max cardepth cardepth-max)
              (min cdrdepth cdrdepth-min)
              (max cdrdepth cdrdepth-max)))))

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

(define (make-scaler dx xmin xmax #:index (index #f))
  ;; If scaler is true just scale the result and don't worry about
  ;; range or it being an integer.  Otherwise do.
  (if (not index)
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
                             #:index #t))
  (define ixcdr (make-scaler delta-cdrdepth cdrdepth-min cdrdepth-max
                             #:index #t))
  (for ([line (in-list data)])
    (match-let ([(list (list cardepth cdrdepth) vals ...) line])
      (for ([grid (in-list grids)]
            [value (in-list vals)])
        (set! (gref grid (ixcar cardepth) (ixcdr cardepth)) value))))
  grids)

;;;; Plotting grids
;;;

;;; Default params
(plot-font-family 'modern)
(plot-width 560)
