;;;; Some variants on searching cons trees
;;;

;;; SBCL will warn about not being able to optimize EQL away.
;;;

(in-package :cl-user)

(needs ("common" :compile t)
       ((:org.tfeb.hax.iterate
         :org.tfeb.hax.simple-loops)
        :use t :compile t))

(defun search/implicit-stack (o thing)
  ;; not TR, good on usual assumptions of lists
  (declare (optimize speed))
  (iterate srch ((x o))
    (typecase x
      (cons
       (srch (car x))
       (srch (cdr x)))
      (null)
      (t
       (when (eql x thing)
         (return-from search/implicit-stack (values x t))))))
  (values nil nil))

(defun search/explicit-stack (o thing &optional (agenda-depth *default-agenda-depth*))
  ;; explicitly iterative.  It is significantly better in both
  ;; implementations not to use vector-push / vector-pop, which is
  ;; a little surprising.
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (if (eql o thing)
        (return-from search/explicit-stack (values thing t))
      (return-from search/explicit-stack (values nil nil))))
  (let ((agenda (make-array agenda-depth)))
    (declare (dynamic-extent agenda)
             (type simple-vector agenda))
    (looping ((this o)
              (depth 0))
      (declare (type array-index depth))
      (typecase this
        (atom
         (when (eql this thing)
           (return-from search/explicit-stack (values this t)))
         (when (zerop depth)
           (return))
         (let ((nd (1- depth)))
           (values (svref agenda nd) nd)))
        (cons
         (when (= depth agenda-depth)
           (error "agenda overflow at ~D" depth))
         (setf (svref agenda depth) (cdr this))
         (values (car this) (1+ depth)))))
    (values nil nil)))

(defun search/explicit-stack/adja (o thing &optional (agenda-depth *default-agenda-depth*)
                                        (growth-factor 1.5))
  ;; explicitly iterative.  It is significantly better in both
  ;; implementations not to use vector-push / vector-pop, which is a
  ;; little surprising.
  ;;
  ;; This version adjusts the stack by assignment: this was slow in LW
  ;; because of a missign declaration
  ;;
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (if (eql o thing)
        (return-from search/explicit-stack/adja (values thing t))
      (return-from search/explicit-stack/adja (values nil nil))))
  (let ((agenda (make-array agenda-depth)))
    (declare (dynamic-extent agenda)
             (type simple-vector agenda))
    (looping ((this o)
              (depth 0))
      (declare (type array-index depth))
      (typecase this
        (atom
         (when (eql this thing)
           (return-from search/explicit-stack/adja (values this t)))
         (when (zerop depth)
           (return))
         (let ((nd (1- depth)))
           (values (svref agenda nd) nd)))
        (cons
         (when (= depth agenda-depth)
           (let ((new-depth (round (* agenda-depth growth-factor))))
             (setf agenda (adjust-array agenda new-depth)
                   agenda-depth new-depth)))
         (setf (svref agenda depth) (cdr this))
         (values (car this) (1+ depth)))))
    (values nil nil)))

(defun search/explicit-stack/adjb (o thing &optional (agenda-depth *default-agenda-depth*)
                                     (growth-factor 1.5))
  ;; explicitly iterative.  It is significantly better in both
  ;; implementations not to use vector-push / vector-pop, which is a
  ;; little surprising.
  ;;
  ;; This version adjusts the stack by binding, which was at one point
  ;; much faster in LW due to a missing declaration
  ;;
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (if (eql o thing)
        (return-from search/explicit-stack/adjb (values thing t))
      (return-from search/explicit-stack/adjb (values nil nil))))
  (iterate srch ((agenda (make-array agenda-depth))
                 (agenda-depth agenda-depth)
                 (current-depth 0)
                 (searching o))
    (declare (dynamic-extent agenda)
             (type simple-vector agenda)
             (type array-index agenda-depth current-depth))
    (looping ((this searching)
              (depth current-depth))
      (declare (type array-index depth))
      (typecase this
        (atom
         (when (eql this thing)
           (return-from search/explicit-stack/adjb (values this t)))
         (when (zerop depth)
           (return))
         (let ((nd (1- depth)))
           (values (svref agenda nd) nd)))
        (cons
         (if (= depth agenda-depth)
             (let ((new-depth (round (* agenda-depth growth-factor))))
               (return-from search/explicit-stack/adjb
                 (srch (adjust-array agenda new-depth)
                       new-depth depth this)))
           (progn
             (setf (svref agenda depth) (cdr this))
             (values (car this) (1+ depth)))))))
    (values nil nil)))

(defun search/consy-stack (o thing)
  (declare (optimize speed))
  (unless (consp o)
    (if (eql o thing)
        (return-from search/consy-stack (values thing t))
      (return-from search/consy-stack (values nil nil))))
  (looping ((this o) (agenda '()))
    (typecase this
      (atom
       (when (eql this thing)
         (return-from search/consy-stack (values this t)))
       (when (null agenda)
         (return))
       (values (first agenda) (rest agenda)))
      (cons
       (values (car this) (cons (cdr this) agenda)))))
  (values nil nil))

(defun ts/s (n car-depth cdr-depth &key (ms nil) (agenda-depth (max (1+ car-depth)
                                                                    *default-agenda-depth*))
               (adjustable-agenda-depth agenda-depth))
  (let ((s (make-car-cdr car-depth cdr-depth 1)))
    (when (not (null s))
      (assert (and (eql (search/implicit-stack s 1) 1)
                   (not (search/implicit-stack s 2))))
      (assert (and (eql (search/explicit-stack s 1 agenda-depth) 1)
                   (not (search/explicit-stack s 2 agenda-depth))))
      (assert (and (eql (search/explicit-stack/adja s 1 adjustable-agenda-depth) 1)
                   (not (search/explicit-stack s 2 adjustable-agenda-depth))))
      (assert (and (eql (search/explicit-stack/adjb s 1 adjustable-agenda-depth) 1)
                   (not (search/explicit-stack s 2 adjustable-agenda-depth))))
      (assert (and (eql (search/consy-stack s 1) 1)
                   (not (search/consy-stack s 2)))))
    (values
     (timing implicit-stack (n :ms ms)
       (search/implicit-stack s 2))
     (timing explicit-stack (n :ms ms)
       (search/explicit-stack s 2 agenda-depth))
     (timing explicit-stack/adja (n :ms ms)
       (search/explicit-stack/adja s 2 adjustable-agenda-depth))
     (timing explicit-stack/adjb (n :ms ms)
       (search/explicit-stack/adjb s 2 adjustable-agenda-depth))
     (timing consy-stack (n :ms ms)
       (search/consy-stack s 2)))))
