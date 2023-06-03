(progn
  "My web server"
  (require 'sb-bsd-sockets)
  (defpackage :server (:use :cl))
  (in-package :server))

(defconstant %pending-connections-queue-length% 1)

(progn
  "Types"

  ;; Inspired by https://stackoverflow.com/a/68974996
  (defun list-of-numbers-p (list)
    "Returns T if LIST only contains numbers."
    (declare (type list list))
    (every #'numberp list))

  (deftype list-of-numbers ()
    `(and list (satisfies list-of-numbers-p))))

(defvar *pages* ()
  "An alist of all routes and pages")

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


;;====== Render state

(defstruct render-state
  (element-ids ())
  (js-code ()))

(defun render-state-collect-id! (render-state id)
  (push id (render-state-element-ids render-state)))

(defun render-state-collect-js! (render-state js)
  (dolist (line js)
    (push line (render-state-js-code render-state))))

(defun render (document-tree state)
  (declare (type (or list string) document-tree)
           (type render-state state))
  (labels ((render-children (children)
             (mapcar (lambda (child)
                       (render child state))
                     children)))
    (cond ((stringp document-tree) document-tree)
          ((html-attribute? document-tree)
           (when (eq (car document-tree) :id)
             (render-state-collect-id! state (cadr document-tree)))
           (format nil "~a=~C~A~C "
                   (car document-tree)
                   #\" (cadr document-tree) #\"))
          ((equal document-tree '(@runtime))
           (make-runtime state))
          ((eq (car document-tree) '@script)
           (render-state-collect-js! state (cdr document-tree)))
          (t (let* ((element-name (car document-tree))
                    (contents (cdr document-tree))
                    (attributes (remove-if-not #'html-attribute? contents))
                    (children (remove-if #'html-attribute? contents)))
               (if (html-self-closing? element-name)
                   (format nil "<~A ~{~A~}>"
                           element-name
                           (render-children attributes)))
               (format nil "<~A ~{~A~}>~{~A~}</~A>"
                       element-name
                       (render-children attributes)
                       (render-children children)
                       element-name))))))

;;====== Runtime

(defun make-runtime (render-state)
  "Generates a runtime JavaScript for the page."
  (with-output-to-string (string)
    (write-string "<script>" string)
    (write-string "D = {};" string)
    (dolist (id (render-state-element-ids render-state))
        (format string "D.~A = document.getElementById('~A');" id id))
    (dolist (js (reverse (render-state-js-code render-state)))
        (format string "~A" js))
    (write-string "</script>" string)))


(defun add-page (route elements)
  (let ((existing-page (get-page route)))
    (if existing-page
        (progn
          (setf (car existing-page) route)
          (setf (cdr existing-page) elements))
        (setf *pages* (acons route elements *pages*)))))

(defun get-page (route)
  (assoc route *pages* :test 'equal))

(defun page (body)
  `(html
    (head
     (meta (:charset "utf-8"))
     (link (:rel "stylesheet") (:href "/style.css"))
     (title "Lisp website"))
    (body
     ,@body
     ,(copyright)
     (@runtime))))

(defmacro defpage (route &body elements)
  `(add-page ,route (lambda () (page ,@elements))))

(defun link (href text)
  (declare (type string href)
           (type string text))
  `(a (:href ,href) ,text))

(defun copyright ()
  `(footer "Copyright 2023 by Maria Zaitseva"))

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

;; https://riptutorial.com/common-lisp/example/19473/reading-and-writing-entire-files-to-and-from-strings#example
(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun get-path (request)
  "Gets a request method, a path, and a protocol version from request."
  (declare (type string request))
  (let* ((pos (position #\return request))
         (line (subseq request 0 pos)))
    (partition line #\space)))

(defun respond (page-generator render-state)
  (declare (type function page-generator)
           (type render-state render-state))
    (make-response
     (render
      (funcall page-generator)
      render-state)))

(defun request-handler-3 (method path protocol)
  (let ((render-state (make-render-state)))
    (let ((page (get-page path))
          (static-resource (format nil "./static/~A" path)))
      (if page
          (progn
            (respond (cdr page)
                     render-state))
          (if (probe-file static-resource)
              (respond (lambda ()
                         (read-file static-resource))
                       render-state)
              (progn
                (respond (cdr (get-page "/notfound"))
                         render-state)))))))

(defun request-handler (request)
  (destructuring-bind (method path protocol) (get-path request)
    (request-handler-3 method path protocol)))

(defun run-server (port)
  (let ((socket (make-socket '(127 0 0 1) port)))
    (unwind-protect
         (runloop socket (lambda (request)
                           (request-handler request)))
      (progn
        (format t "Closing connection~%")
        (sb-bsd-sockets:socket-close socket)))))

(defpage "/notfound"
  `((h1 "404: Page not found")
    (p "It's not here...")))

(defpage "/"
  `((h1 "Maria Zaitseva")
    (p "I'm a software developer located at Novosibirsk, Russia.")
    (p "I mostly use Javascript and Swift in my projects.")
    (p "This website is written in Common Lisp.")
    (p ,(link "/bio" "See my biography."))
    (p ,(link "/clicker" "Play a clicker game."))
    (p "See a page that " ,(link "/notexist" "doesn't exit."))))

(defpage "/bio"
  `((h1 "My biography")
    (p ,(link "/" "Back"))
    (p "I was born at March 13, 1994.")
    (p "I think I got my first computer when I was 5.")
    (p "It was a Famiclone machine with a keyboard capable of running BASIC programs.")
    (p "The machine broke soon after, but I still had the BASIC "
       "programming manual lying around for years")
    (p "I kept reading and re-reading the book for many many hours, "
       "imagining what would it be like if I could program a real computer.")))

(defpage "/clicker"
  `((h1 "Clicker game")
    (@script
     "function topCounter() {"
     "  return Number(D.counter.textContent);"
     "}"
     "function bottomCounter() {"
     "  return Number(D.nounter.textContent);"
     "}"
     "function setTopCounter(value) {"
     "  D.counter.textContent = value;"
     "}"
     "function setBottomCounter(value) {"
     "  D.nounter.textContent = value;"
     "}"
     )
    (p ,(link "/" "Back"))
    (p "Yes, it's written in Common Lisp.")
    (p (button "Decrease"
               (:onclick "setTopCounter(topCounter() - bottomCounter())"))
       (span (:id "counter") "0")
       (button "Increase"
               (:onclick "setTopCounter(topCounter() + bottomCounter())")))
    (p (button "Decrease"
               (:onclick "setBottomCounter(bottomCounter() - 1)"))
       (span (:id "nounter") "1")
       (button "Increase"
               (:onclick "setBottomCounter(bottomCounter() + 1)")))
    (p "No, it's not the most fun part about it.")))

(run-server 8092)
