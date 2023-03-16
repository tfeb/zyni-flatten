;;;; Timing flatten variants
;;;

(in-package :cl-user)

(needs ("common" :compile t)
       ((:org.tfeb.hax.iterate
         :org.tfeb.hax.collecting
         :org.tfeb.hax.simple-loops)
        :use t :compile t))

(defun flatten/implicit-stack (o)
  (declare (optimize speed))
  ;; not TR, better on usual assumptions, no reverse
  (collecting
    (iterate ftn ((x o))
      (typecase x
        (cons
         (ftn (car x))
         (ftn (cdr x)))
        (null)
        (t (collect x))))))

(defun flatten/explicit-stack (o &optional (agenda-depth *default-agenda-depth*))
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (return-from flatten/explicit-stack o))
  (let ((agenda (make-array agenda-depth)))
    (declare (dynamic-extent agenda)    ;?
             (type simple-vector agenda))
    (collecting
      (looping ((this o)
                (depth 0))
        (declare (type array-index depth))
        (typecase this
          (atom
           (unless (null this) (collect this))
           (when (zerop depth)
             (return))
           (let ((nd (1- depth)))
             (values (svref agenda nd) nd)))
          (cons
           (when (= depth agenda-depth)
             (error "agenda overflow at ~D" depth))
           (setf (svref agenda depth) (cdr this))
           (values (car this) (1+ depth))))))))

(defun flatten/explicit-stack/adja (o &optional (agenda-depth *default-agenda-depth*)
                                      (growth-factor 1.5))
  ;; Adjust array by assignment
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (return-from flatten/explicit-stack/adja o))
  (let ((agenda (make-array agenda-depth)))
    (declare (dynamic-extent agenda)    ;?
             (type simple-vector agenda))
    (collecting
      (looping ((this o)
                (depth 0))
        (declare (type array-index depth))
        (typecase this
          (atom
           (unless (null this) (collect this))
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
           (values (car this) (1+ depth))))))))

(defun flatten/explicit-stack/adjb (o &optional (agenda-depth *default-agenda-depth*)
                                      (growth-factor 1.5))
  ;; Adjust array by binding
  (declare (optimize speed)
           (type array-index agenda-depth))
  (unless (consp o)
    (return-from flatten/explicit-stack/adjb o))
  (iterate ftn ((agenda (make-array agenda-depth))
                (agenda-depth agenda-depth)
                (current-depth 0)
                (processing o))
    (declare (dynamic-extent agenda)
             (type simple-vector agenda)
             (type array-index agenda-depth current-depth))
    (collecting
      (looping ((this processing)
                (depth current-depth))
        (declare (type array-index depth))
        (typecase this
          (atom
           (unless (null this) (collect this))
           (when (zerop depth)
             (return))
           (let ((nd (1- depth)))
             (values (svref agenda nd) nd)))
          (cons
           (if (= depth agenda-depth)
               (let ((new-depth (round (* agenda-depth growth-factor))))
                 (return (ftn (adjust-array agenda new-depth)
                              new-depth depth this)))
             (progn
               (setf (svref agenda depth) (cdr this))
               (values (car this) (1+ depth))))))))))

(defun flatten/consy-stack (o)
  (declare (optimize speed))
  (unless (consp o)
    (return-from flatten/consy-stack o))
  (collecting
    (looping ((this o) (agenda '()))
      (typecase this
        (atom
         (unless (null this) (collect this))
         (when (null agenda)
           (return))
         (values (first agenda) (rest agenda)))
        (cons
         (values (car this) (cons (cdr this) agenda)))))))

(defun ts/f (n car-depth cdr-depth &key (ms nil) (agenda-depth (max (1+ car-depth)
                                                                    *default-agenda-depth*))
               (adjustable-agenda-depth agenda-depth))
  (let ((s (make-car-cdr car-depth cdr-depth 1)))
    (assert (= (length (flatten/implicit-stack s)) cdr-depth))
    (assert (= (length (flatten/explicit-stack s agenda-depth)) cdr-depth))
    (assert (= (length (flatten/explicit-stack/adja
                        s adjustable-agenda-depth)) cdr-depth))
    (assert (= (length (flatten/explicit-stack/adjb
                        s adjustable-agenda-depth)) cdr-depth))
    (assert (= (length (flatten/consy-stack s)) cdr-depth))
    (values
     (timing implicit-stack (n :ms ms)
       (flatten/implicit-stack s))
     (timing explicit-stack (n :ms ms)
       (flatten/explicit-stack s agenda-depth))
     (timing explicit-stack/adjust (n :ms ms)
       (flatten/explicit-stack/adja s adjustable-agenda-depth))
     (timing explicit-stack/adjust (n :ms ms)
       (flatten/explicit-stack/adjb s adjustable-agenda-depth))
     (timing consy-stack (n :ms ms)
       (flatten/consy-stack s)))))
