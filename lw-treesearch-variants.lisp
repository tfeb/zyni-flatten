;;;; Two variants on searching cons trees, one specific to LW
;;;

(in-package :cl-user)

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "LispWorks only"))

(needs ("common" :compile t)
       ((:org.tfeb.hax.iterate
         :org.tfeb.hax.simple-loops
         :org.tfeb.hax.metatronic
         :org.tfeb.lw.allowing-stack-extensions)
        :use t :compile t))

(defun search/implicit-stack/extend (o thing &optional (limit t))
  ;; not TR, good on usual assumptions of lists, but allow stack
  ;; extension if need be.
  (declare (optimize speed))
  (allowing-stack-extensions (:limit limit)
    (iterate srch ((x o))
      (typecase x
        (cons
         (srch (car x))
         (srch (cdr x)))
        (null)
        (t
         (when (eql x thing)
           (return-from search/implicit-stack/extend (values x t)))))))
  (values nil nil))

(defun search/explicit-stack/extend (o thing &optional (agenda-depth *default-agenda-depth*)
                                (growth-factor 1.5))
  ;; explicitly iterative.  It is significantly better in both
  ;; implementations not to use vector-push / vector-pop, which is a
  ;; little surprising.
  ;;
  ;; This version adjusts the stack by binding, which was at one point
  ;; much faster in LW due to a missing declaration
  ;;
  ;; This is search/explicit-stack/adjb in treesearch-variants
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (if (eql o thing)
        (return-from search/explicit-stack/extend (values thing t))
      (return-from search/explicit-stack/extend (values nil nil))))
  (iterate srch ((agenda (make-array agenda-depth))
                 (agenda-depth agenda-depth)
                 (current-depth 0)
                 (searching o))
    (declare (type simple-vector agenda)
             (type array-index agenda-depth current-depth))
    (looping ((this searching)
              (depth current-depth))
      (declare (type array-index depth))
      (typecase this
        (atom
         (when (eql this thing)
           (return-from search/explicit-stack/extend (values this t)))
         (when (zerop depth)
           (return))
         (let ((nd (1- depth)))
           (values (svref agenda nd) nd)))
        (cons
         (if (= depth agenda-depth)
             (let ((new-depth (round (* agenda-depth growth-factor))))
               (return-from search/explicit-stack/extend
                 (srch (adjust-array agenda new-depth)
                       new-depth depth this)))
           (progn
             (setf (svref agenda depth) (cdr this))
             (values (car this) (1+ depth)))))))
    (values nil nil)))

(defun ts/s/lw (n car-depth cdr-depth &key (unit ':s)
                  (initial-agenda-depth *default-agenda-depth*))
  (let ((s (make-car-cdr car-depth cdr-depth 1)))
    (when (not (null s))
      (assert (and (eql (search/implicit-stack/extend s 1) 1)
                   (not (search/implicit-stack/extend s 2))))
      (assert (and (eql (search/explicit-stack/extend s 1 initial-agenda-depth) 1)
                   (not (search/explicit-stack/extend s 2 initial-agenda-depth)))))
    (values
     (timing implicit-stack/extend (n :unit unit)
       (search/implicit-stack/extend s 2))
     (timing explicit-stack/extend (n :unit unit)
       (search/explicit-stack/extend s 2 initial-agenda-depth)))))
