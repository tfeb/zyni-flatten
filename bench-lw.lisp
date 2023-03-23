;;;; Run LW benchmarks
;;;

(in-package :cl-user)

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "LispWorks only"))

(needs ("lw-treesearch-variants"
        :compile t))

(defun bench/lw (iters note &rest args &key &allow-other-keys)
  (let ((out (make-pathname :name
                            (format nil "lw-treesearch-~D-~@[~A-~]lispworks-~A"
                                    iters
                                    note
                                    (string-downcase (machine-type)))
                            :type "ldat"
                            :defaults (or *load-truename*
                                          *default-pathname-defaults*))))
    (format t "~&LW treesearch ~D to ~A~%" iters out) (finish-output)
    (apply #'bench-variants #'ts/s/lw iters :to-file out args)
    out))
