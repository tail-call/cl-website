;; Structs

(defstruct gzip-flags
  (FTEXT nil)
  (FHCRC nil)
  (FEXTRA nil)
  (FNAME nil)
  (FCOMMENT nil))

(defstruct gzip-extra
  (SI "XX")
  (data (make-array 0)))

(defstruct gzip-header
  (ID0 #x1F)
  (ID1 #x8B)
  (CM 8)
  (FLG (make-gzip-flags))
  (MTIME 0)
  (XFL 4)                               ; Fastest compression, see RFC 1952, p.7
  (OS 3)                                ; Unix, see RFC 1952, p.7
  (XLEN 0)
  (CRC32 0)
  (ISIZE 0)
  (extra (make-gzip-extra)))

(defconstant %bad-number% (integer->bit-vector #xEDB88320)
  "Used in CRC computation.")

;; From https://lispforum.com/viewtopic.php?p=6269#p6269
(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun bit-vector->integer (bit-vector)
  "Creates a positive integer from a bit-vector."
  (let ((result 0)
        (multiplier 1))
    (loop for i
          from (1- (length bit-vector))
          downto 0
          do (let ((element (aref bit-vector i)))
               (setf result (+ result (* multiplier element)))
               (setf multiplier (* 2 multiplier))))
    result))

(dotimes (k (1- k) 0)
  (print k))

(defun drop-last (sequence count)
    (remove-if (lambda (-) t)
               sequence
               :count count
               :from-end t))

(defun bit-pad-left (sequence item count)
  (let ((padding (make-array count :initial-element item)))
    (concatenate '(vector bit) padding sequence)))

(defun force-n-bits (n bit-vector)
  (bit-pad-left bit-vector 0 (- n (length bit-vector))))

(defun integer->bit32 (integer)
  (force-n-bits 32 (integer->bit-vector integer)))

(defun bit-32-shift-right (count bit-array)
  (force-n-bits 32 (drop-last bit-array count)))

(defun make-crc-table ()
  "Make the table for a fast CRC."
  (let ((c (fill-32 0))
        (crc-table (make-array 256)))
    (dotimes (n 256)
      (setf c (integer->bit32 n))
      (dotimes (k 8)
        (if (equal (bit-and c (integer->bit32 1)) (integer->bit32 1))
            (setf c (bit-xor %bad-number%
                             (bit-32-shift-right 1 c))))
            (setf c (bit-32-shift-right 1 c )))
      (setf (aref crc-table n) c)
      (setf *crc-table-computed* t))
    crc-table))

(defun fill-32 (bit)
  (make-array 32 :element-type 'bit :initial-element bit))

(defun update-crc (crc sequence crc-table)
  "Update a running crc with the bytes buf[0..len-1] and return
   the updated crc. The crc should be initialized to zero.
   post-conditioning (one's complement) is performed within this
   function so it shouldn't be done by the caller."
  (let ((c (bit-xor crc (fill-32 1))))
    (dotimes (n (length sequence))
      (setf c (bit-xor (aref crc-table (bit-vector->integer
                                        (bit-and (force-n-bits 32 #*11111111)
                                                 (bit-xor c (force-n-bits 32 (integer->bit-vector (aref sequence n)))))))
                       (force-n-bits 32 (drop-last c 8))))
    (bit-xor c (fill-32 1)))))

(defun crc-32 (data)
  (bit-vector->integer (update-crc (fill-32 0) data (make-crc-table))))

(crc-32 #(1 2 3 4))

(c

(progn
  (format t "~CType yes or no: " #\linefeed)
  (let ((input (read)))
    (if (eq input 'yes)
        (format t "You typed yes.")
        (format t "You didn't type yes."))))
