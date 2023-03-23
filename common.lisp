;;;; Shared
;;;

(in-package :cl-user)

(needs ((:org.tfeb.hax.iterate
         :org.tfeb.hax.collecting
         :org.tfeb.hax.metatronic)
        :use t :compile t))

(deftype array-index ()
  `(integer 0 (,array-dimension-limit)))

(defvar *default-agenda-depth* 100)

(defun make-car-cdr (car-depth cdr-depth leaf)
  (collecting
    (dotimes (i cdr-depth)
      (iterate cc ((chain leaf)
                   (depth 0))
        (if (= depth car-depth)
            (collect chain)
            (cc (list chain) (1+ depth)))))))

(defvar *report-timing* t)

(defmacro/m timing (name (iters &key (unit ':s) (report '*report-timing*))
                         &body code)
  ;; The point of this is so that TIME reports just a useful name for
  ;; the thing it is timing rather than all sorts of tedious detail
  `(flet ((,name ()
            (let ((<start> (get-internal-real-time))
                  (<iters> ,iters))
              (dotimes (<i> <iters> (float (*
                                            (/ (- (get-internal-real-time)
                                                  <start>)
                                               internal-time-units-per-second
                                               <iters>)
                                            (ecase ,unit
                                              ((:s :second :seconds) 1)
                                              ((:ms :millisecond :milliseconds) 1000)
                                              ((:us :microsecond :microseconds) 1000000)
                                              ((:ns :nanosecond :nanoseconds) 1000000000)))))
                ,@code))))
     (if ,report
         #+LispWorks(extended-time (,name))
         #-LispWorks(time (,name))
         (,name))))

(defun timing-probably-sane (&key (allowed-error 0.1)
                                  (report nil))
  (flet ((about= (value expected)
           (<= (* expected (- 1 allowed-error))
               value
               (* expected (+ 1 allowed-error)))))
    (and (about= (timing 1s (1 :report report) (sleep 1)) 1.0)
         (about= (timing 1s (10 :report report) (sleep 1)) 1.0)
         (about= (timing 1s (1 :unit :ms :report report) (sleep 1)) 1000.0)
         (about= (timing 1s (1 :unit :us :report report) (sleep 1)) 1000000.0))))

(defun bench-variants (ts n &key
                         (carmin 0) (carmax 300) (carstep 10)
                         (cdrmin 0) (cdrmax 300) (cdrstep 10)
                         (unit :ms) (report-timing nil) (report-progress t)
                         (to-file nil))
  (let ((*report-timing* report-timing))
    (let ((results (collecting
                     (do ((cardepth carmin (+ cardepth carstep)))
                         ((> cardepth carmax))
                       (when report-progress
                         (format t "~&car depth ~5D" cardepth)
                         (finish-output))
                       (do ((cdrdepth cdrmin (+ cdrdepth cdrstep)))
                           ((> cdrdepth cdrmax))
                         (when report-progress
                           (format t ".")
                           (finish-output))
                         (collect `((,cardepth ,cdrdepth)
                                    ,@(multiple-value-list
                                       (funcall ts n cardepth cdrdepth :unit unit)))))))))
      (if to-file
        (with-standard-io-syntax
          (with-open-file (o to-file :direction ':output :if-exists ':supersede)
            (pprint results o)
            (terpri o))
          to-file)
        results))))
