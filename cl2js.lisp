(defun string-append (&rest strings)
  (apply #'concatenate `(string ,@strings)))

(defun symbol->js-identifier (symbol)
  (with-output-to-string (output)
    (let ((next-is-uppercase nil))
      (labels ((iterate (str func)
                 (map nil func str))
               (emit (char)
                 (princ char output))
               (str (symbol)
                 (string-downcase (symbol-name symbol))))
       (iterate (string-append "__" (str symbol))
                (lambda (char)
                  (cond (next-is-uppercase
                         (emit (char-upcase char))
                         (setq next-is-uppercase nil))
                        ((char-equal char #\-)
                         (emit #\_)
                         (setq next-is-uppercase t))
                        (t
                         (emit char)))))))))

(defun list->js-list (params printer)
  (with-output-to-string (output)
    (progn
      (princ "(" output)
      (map nil (lambda (item)
                 (funcall printer item)
                 (princ ", " output))
           params)
      (princ "__)" output))))

(defun list->js-parameter-list (params)
  (list->js-list params #'symbol->js-identifier))

(defun for-each-plist (func plist)
  (destructuring-bind
      (name value . rest)
      plist
    (funcall func name value)
    (for-each-plist rest func plist)))

(for-each-plist (lambda (name value)
                  (format t "name=~a value=~a~%" name value))
                '(:FILL-POINTER 0 :INITIAL-ELEMENT NIL)
                )

(defun select-non-keywords (args)
  (loop :for item :in args
        :until (keywordp item)
        :collect item))

(select-non-keywords '(0 :FILL-POINTER 0 :INITIAL-ELEMENT NIL))

(with-output-to-string (output)
  (with-open-file (input "data.lisp")
    (labels
        ((next-form ()
           (read input))
         (emit (format-string &rest format-params)
           (apply #'format `(,output ,format-string ,@format-params)))
         (parse-function-call (expression)
           (destructuring-bind
               (name . params)
               expression
             (emit "~a(" (symbol->js-identifier name))
             (emit ");~%")))
         (parse-expression (expression)
           (etypecase expression
             (list (parse-function-call expression))
             (number (emit "~a" expression))))
         (parse-statement (statement indent)
           (dotimes (i indent)
             (emit "    "))
           (parse-expression statement))
         (parse-block (body)
           (dolist (statement body)
             (parse-statement statement 1)))
         (parse-defun (items)
           (destructuring-bind
               (name args . body)
               items
             (emit "function ~a~a{~%"
                   (symbol->js-identifier name)
                   (list->js-parameter-list args))
             (parse-block body)
             (emit "}~%")))
         (parse-list (items)
           (ecase (first items)
             ((defun)
              (parse-defun (rest items)))))
         (parse (form)
           (etypecase form
             (list (parse-list form)))))
      (parse (next-form))
      )))

(let ((a (list 1 2 3)))
  `(a ,@a))
(apply 
