;;;; Run all the benchmarks to files
;;;

(in-package :cl-user)

(needs (("treesearch-variants"
         "flatten-variants")
        :compile t))

(defun bench (iters &rest args &key &allow-other-keys)
  (flet ((make-ldat-pathname (for)
           (make-pathname :name (format nil "~A-~D-~A-~A"
                                        for
                                        iters
                                        (string-downcase
                                         (lisp-implementation-type))
                                        (string-downcase (machine-type)))
                          :type "ldat"
                          :defaults (or *load-truename* *default-pathname-defaults*))))
    (let ((treesearch-file (make-ldat-pathname "treesearch"))
          (flatten-file (make-ldat-pathname "flatten")))
      (format t "~&Treesearch ~D to ~A~%" iters treesearch-file) (finish-output)
      (apply #'bench-variants #'ts/s iters :to-file treesearch-file args)
      (format t "~&Flatten ~D to ~A~%" iters flatten-file) (finish-output)
      (apply #'bench-variants #'ts/f iters :to-file flatten-file args)
      (values treesearch-file flatten-file))))
