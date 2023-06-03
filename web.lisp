(progn
  "My web server"
  (require 'sb-bsd-sockets)
  (defpackage :server (:use :cl))
  (in-package :server)
  (defconstant %pending-connections-queue-length% 1))

(progn
  "Types"

  ;; Inspired by https://stackoverflow.com/a/68974996
  (defun list-of-numbers-p (list)
    "Returns T if LIST only contains numbers."
    (declare (type list list))
    (every #'numberp list))

  (deftype list-of-numbers ()
    `(and list (satisfies list-of-numbers-p))))

(defun make-socket (address port)
  "Returns a socket listening to an ADDRESS at specified PORT."
  (declare (type list-of-numbers address)
           (type number port))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    ;; Reuse address
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    ;; Bind socket to an address
    (sb-bsd-sockets:socket-bind socket address port)
    ;; Listen for connections
    (sb-bsd-sockets:socket-listen socket %pending-connections-queue-length%)
    socket))

(defun make-buffer (length)
  (declare (type fixnum length))
  (make-array length :element-type 'character
                     :initial-element #\nul))

(defun accept-one-stream (socket handler)
  "Accepts a client and sends back response"
  (declare (type sb-bsd-sockets:socket socket)
           (type function handler))
  (let* ((client (sb-bsd-sockets:socket-accept socket))
         (buffer (make-buffer 512)))
    (unwind-protect
         (let* ((request (sb-bsd-sockets:socket-receive client buffer nil))
                (response (funcall handler request)))
           (print response)
           (sb-bsd-sockets:socket-send client
                                       response
                                       (length response)
                                       :external-format :utf-8))
      (sb-bsd-sockets:socket-close client))))

(defun runloop (socket handler)
  (declare (type sb-bsd-sockets:socket socket)
           (type function handler))
  (accept-one-stream socket handler)
  (runloop socket handler))

(defun html-attribute? (attr)
  (and (consp attr)
       (keywordp (car attr))
       (stringp (cadr attr))
       t))

(defun html-self-closing? (element-name)
  (or (eq element-name "meta")
      (eq element-name "img")))

(defun render (document-tree)
  (declare (type (or list string) document-tree))
  (cond ((stringp document-tree) document-tree)
        ((html-attribute? document-tree)
         (format nil "~a=~C~A~C "
                 (car document-tree)
                 #\" (cadr document-tree) #\"))
        (t (let* ((element-name (car document-tree))
                  (contents (cdr document-tree))
                  (attributes (remove-if-not #'html-attribute? contents))
                  (children (remove-if #'html-attribute? contents)))
             (if (html-self-closing? element-name)
                 (format nil "<~A ~{~A~}>"
                         element-name
                         (mapcar #'render attributes)))
                 (format nil "<~A ~{~A~}>~{~A~}</~A>"
                         element-name
                         (mapcar #'render attributes)
                         (mapcar #'render children)
                         element-name)))))

(defun page (body)
  `(html
    (head
     (meta (:charset "utf-8"))
     (title "Lisp website")
     (body
      ,@body))))

(defun main-page ()
  (page `((h1 "Common Lisp did this")
          (p "You are at main page. " (a (:href "/demo") "See a path demo."))
          (p "See a page that " (a (:href "/notexist") "doesn't exit."))
          (p "I like it " (strong "a lot") "."))))

(defun not-found-page ()
  (page `((h1 "404: Page not found")
          (p "It's not here..."))))

(defun demo-page ()
  (page `((h1 "Link demo")
          (p "Welcome to this linked page!")
          (p (a (:href "/") "Back")))))

(defun make-response (body)
  (let ((lines (list
                "HTTP/1.1 200 OK"
                (format nil "Content-Length: ~A" (length body))
                "Content-Type: text/html; charset=utf-8"
                ""
                body)))
    (format nil "~{~A
~}" lines)))

(defun partition (sequence splitter)
  (let ((left-side 0)
        (right-side 0)
        (items ()))
    (labels ((emit ()
               (setf items (cons (subseq sequence left-side right-side)
                                 items))))
      (dotimes (i (length sequence))
        (setf right-side i)
        (let ((item (aref sequence right-side)))
          (when (eq item splitter)
            (emit)
            (incf right-side)
            (setf left-side right-side))))
      (incf right-side)
      (emit)
      (reverse items))))

(destructuring-bind (method path protocol) (partition "GET / HTTP/1.1" #\space)
  (format nil "Method: ~A~%Path: ~A~%Protocol:~A~%" method path protocol))

(defun get-path (request)
  (declare (type string request))
  (let* ((pos (position #\return request))
         (line (subseq request 0 pos)))
    (partition line #\space)))

(defun request-handler (request)
  (destructuring-bind (- path -) (get-path request)
    (labels ((respond (page-generator)
               (let ((text (render (funcall page-generator))))
                 (make-response text))))
      (cond ((equal path "/demo") (respond #'demo-page))
            ((equal path "/") (respond #'main-page))
            (t (respond #'not-found-page))))))

(progn
  "Entry point"
  (let ((socket (make-socket '(127 0 0 1) 8091)))
    (unwind-protect
         (runloop socket (lambda (request)
                           (request-handler request)))
      (progn
        (format t "Closing connection~%")
        (sb-bsd-sockets:socket-close socket)))))