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

(defmacro/m timing (name (iters &key (unit ':s)) &body code)
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
     (if *report-timing*
         #+LispWorks(extended-time (,name))
         #-LispWorks(time (,name))
         (,name))))

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
                         (multiple-value-bind (implicit explicit adja adjb consy)
                             (funcall ts n cardepth cdrdepth :unit unit)
                           (collect `((,cardepth ,cdrdepth) ,implicit ,explicit
                                      ,adja ,adjb ,consy))))))))
      (if to-file
        (with-standard-io-syntax
          (with-open-file (o to-file :direction ':output :if-exists ':supersede)
            (pprint results o)
            (terpri o))
          to-file)
        results))))
