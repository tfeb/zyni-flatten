;;;; From Zyni
;;;

(in-package :cl-user)

(needs ((:org.tfeb.hax.iterate
         :org.tfeb.hax.collecting
         :org.tfeb.hax.simple-loops)
        :use t :compile t))

(defun flatten (o)
  ;; original terrible one
  (iterate ftn ((x o) (accumulator '()))
    (typecase x
      (null accumulator)
      (cons (ftn (car x) (ftn (cdr x) accumulator)))
      (t (cons x accumulator)))))

(defun flatten (o)
  ;; not TR but better on usual assumptions
  (nreverse
   (iterate ftn ((x o) (accumulator '()))
     (typecase x
       (null accumulator)
       (cons (ftn (cdr x) (ftn (car x) accumulator)))
       (t (cons x accumulator))))))

(defun flatten (o)
  ;; not TR, better on usual assumptions, no reverse
  (collecting
    (iterate ftn ((x o))
      (typecase x
        (cons
         (ftn (car x))
         (ftn (cdr x)))
        (null)
        (t (collect x))))))

(defun flatten (o)
  ;; pure TR
  (iterate ftn ((agenda (list o))
                (accumulator '()))
    (if (null agenda)
        ;; can write own reverse as tail recursive of course if wish
        ;; to be pure of heart
        (nreverse accumulator)
      (destructuring-bind (this . more) agenda
        (typecase this
          (null
           (ftn more accumulator))
          (cons
           (ftn (list* (car this) (cdr this) more) accumulator))
          (t
           (ftn more (cons this accumulator))))))))

(defun flatten (o)
  ;; pure TR, no reverse
  (collecting
    (iterate ftn ((agenda (list o)))
      (when (not (null agenda))
        (destructuring-bind (this . more) agenda
          (typecase this
            (null
             (ftn more))
            (cons
             (ftn (list* (car this) (cdr this) more)))
            (t
             (collect this)
             (ftn more))))))))


(defun flatten (o)
  ;; pure TR, no reverse
  (collecting
    (iterate ftn ((agenda (list o)))
      (when (not (null agenda))
        (destructuring-bind (this . more) agenda
          (typecase this
            (null
             (ftn more))
            (cons
             (ftn (list* (car this) (cdr this) more)))
            (t
             (collect this)
             (ftn more))))))))

(defun flatten (o)
  ;; Iterative
  (collecting
    (looping ((agenda (list o)))
      (when (null agenda)
        (return))
      (destructuring-bind (this . more) agenda
        (typecase this
          (null more)
          (cons (list* (car this) (cdr this) more))
          (t (collect this) more))))))
