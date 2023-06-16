(defun parse (source)
  (with-input-from-string (input source)
    (let ((last-input nil)
          (push-back ())
          (variables ()))
      (labels (;; Utilities

               (neq (a b)
                 (not (eq a b)))
               (errorf (format-message &rest format-params)
                 (error (format nil "~A expected, got ~A instead"
                                format-message format-params)))

               ;; Reading

               (read-token ()
                 (cond ((null push-back)
                        (setf last-input (read input)))
                       (t
                        (setf last-input (pop push-back))
                 last-input)))
               (unread-token (token)
                 (push token push-back))
               (is-next-token? (token)
                 (read-token)
                 (let ((result (eq last-input token)))
                   (unread-token last-input)
                   result))
               (read-symbol ()
                 (if (symbolp (read-token))
                     last-input
                     (errorf "A symbol expected, got ~a instead"
                             last-input)))
               (read-literal (literal)
                 (when (neq literal (read-token))
                   (errorf "Literal ~a expected, got ~a instead"
                           literal last-input))
                 literal)

               ;; Parsing

               (parse-expression ()
                 (let* ((var-1 (read-symbol))
                        (op (read-symbol))
                        (var-2 (read-symbol))
                        (value-1 (getf variables var-1))
                        (value-2 (getf variables var-2)))
                   (setf (getf variables 'return-value)
                         (case op
                           ((+)
                            (+ value-1 value-2))
                           ((-)
                            (- value-1 value-2))
                           ((*)
                            (* value-1 value-2))
                           ((/)
                            (/ value-1 value-2))))))
               (parse-statement (token)
                 (case token
                   ((let)
                    (let* ((variable-name (read-symbol))
                           (- (read-literal '=))
                           (value (read-token)))
                      (setf (getf variables variable-name) value)))
                   ((return)
                    (parse-expression))))
               (parse-program (token)
                 (case token
                   ((begin)
                    (read-literal '{)
                    (loop :while (not (is-next-token? '})) :do
                      (parse-statement (read-token)))
                    (read-literal '})))))
        (print (is-next-token? '{))
        (parse-program (read-token))
        variables))))

(parse
"begin {
    let x = 42
    let x = 2
    return x * y
}")

(parse
"begin {
    let a = 1
    let b = 99
    return b / b
}")
