;; C-c C-c this first
(progn
  (defpackage :my-package
    (:use :cl)
    (:export #:hello))

  (in-package :my-package)

  (defun hello ()
    (format t "yesss hello")))

;; Then C-c C-c this
(progn
  (in-package :cl-user)
  (my-package:hello))
