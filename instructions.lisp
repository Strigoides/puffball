;;;; Defines the hash-table containing all standard instructions.
;;;; Each instruction is a function, which is passed a single
;;;; argument; the ip that is executing it

(in-package :puffball)

(defvar *funge-98-instructions* (make-hash-table)
  "Standard instruction set for Funge-98")

(defmacro define-funge-instruction (name &body body)
  "Defines a funge instruction corresponding to the character NAME.
   Instructions have two arguments: IP, which is bound to the ip executing the
   instruction, and F-SPACE, which is bound to the funge-space that the ip
   executing the instruction is within. The return value of the function is
   assigned to the ip that executed it, so destructively modifying the ip passed
   in is perfectly acceptable"
  `(setf (gethash ,name *funge-98-instructions*)
         (lambda (ip f-space)
           ,@(if (stringp (car body)) ; Put docstrings first
               `(,(car body)
                  (declare (ignorable f-space))
                 ,@(cdr body))
               `((declare (ignorable f-space))
                 ,@body)))))

;;; Instructions "0" to "9", and "a" to "f"
;;; The more natural DOTIMES or LOOP doesn't work here, as it closes over the
;;; same integer 16 times, and causes all 16 instructions to push 16 onto
;;; the stack
(mapc
  (lambda (n char)
    (setf (gethash char *funge-98-instructions*) 
          (lambda (ip f-space)
            (declare (ignore f-space))
            (push n (top-stack ip))
            ip)))
  (loop for x from 0 to 15 collecting x)
  (coerce "0123456789abcdefg" 'list))

;;; Arithmetic
(define-funge-instruction #\+
  "Pop the top two stack values and add them together"
  (push (+ (pop-stack ip)
           (pop-stack ip))
        (top-stack ip))
  ip)

(define-funge-instruction #\-
  "Pop the top two stack values and subtract the first from the second"
  (push (- (- (pop-stack ip)
              (pop-stack ip)))
        (top-stack ip))
  ip)

(define-funge-instruction #\*
  "Pop the top two stack values and multiply them together"
  (push (* (pop-stack ip)
           (pop-stack ip))
        (top-stack ip))
  ip)

(define-funge-instruction #\/
  "Pop the top two stack values and divide the second by the first"
  (let ((a (pop-stack ip))
        (b (pop-stack ip)))
    (push (if (zerop a)
            0
            (floor b a))
          (top-stack ip))
    ip))

(define-funge-instruction #\%
  "Pop the top two stack values and find the remainder of dividing the second
   by the first"
  (let ((a (pop-stack ip))
        (b (pop-stack ip)))
    (push (if (zerop a)
            0
            (mod b a))
          (top-stack ip))
    ip))

;;; Control flow
(define-funge-instruction #\@
  "Kill the current IP"
  (declare (ignore ip))
  nil)

(define-funge-instruction #\Space
  "NOP"
  ;; TODO: Implement backtrack wrapping; " " shouldn't be a NOP, as it should
  ;; execute in "no time at all", as far as concurrency is concerned
  ip)

(define-funge-instruction #\z
  "NOP"
  ip)

(define-funge-instruction #\;
  "Skip instructions until the next ;"
  ;; TODO: This should also take "no time at all" as far as concurrency is
  ;; concerned
  (setf (ip-location ip)
        (vector-minus
          (next-instruction (ip-location ip)
                            (ip-delta ip)
                            f-space)
          (ip-delta ip)))
  ip)

(define-funge-instruction #\<
  "Change the DELTA of the IP to point to the left"
  (setf (ip-delta ip)
        #(-1 0))
  ip)

(define-funge-instruction #\v
  "Change the DELTA of the IP to point down"
  (setf (ip-delta ip)
        #(0 1))
  ip)

(define-funge-instruction #\^
  "Change the DELTA of the IP to point up"
  (setf (ip-delta ip)
        #(0 -1))
  ip)

(define-funge-instruction #\>
  "Change the DELTA of the IP to point to the right"
  (setf (ip-delta ip)
        #(1 0))
  ip)

(define-funge-instruction #\#
  "`Tramponline' instruction; jump over one cell"
  (move-ip ip))

(define-funge-instruction #\r
  "Reverse the direction of travel"
  (setf (ip-delta ip)
        (vector-times-int (ip-delta ip) -1))
  ip)

(define-funge-instruction #\[
  "Turn to the left"
  (setf (ip-delta ip)
        (vector (elt (ip-delta ip) 1)
                (- (elt (ip-delta ip) 0))))
  ip)

(define-funge-instruction #\]
  "Turn to the right"
  (setf (ip-delta ip)
        (vector (- (elt (ip-delta ip) 1))
                (elt (ip-delta ip) 0)))
  ip)

(define-funge-instruction #\w
  "Pop two values. If the second is smaller, turn left. If the first is
   smaller, turn right. If they're the same, keep going forward"
  (let ((a (pop-stack ip))
        (b (pop-stack ip)))
    (funcall (gethash (cond
                        ((> a b) #\[)
                        ((< a b) #\])
                        (t #\Space))
                      *funge-98-instructions*)
             ip f-space)))

(define-funge-instruction #\k
  "Pop a value `n' off the stack, move forward once, and then perform the
   instruction under the ip n times"
  (let ((n (pop-stack ip))
        (instruction
          (gethash (char-at-vector
                     f-space
                     (do ((location (vector-+ (ip-location ip)
                                              (ip-delta ip))
                                    (vector-+ location
                                              (ip-delta ip))))
                       ((not (member (char-at-vector f-space location)
                                     '(#\Space #\;)))
                        location)))
                   *funge-98-instructions*))) 
    (if (zerop n)
      (move-ip ip)
      (loop repeat n do
            (setf ip (funcall instruction ip f-space)))))
  ip)

(define-funge-instruction #\j
  "Pop a value n, and jump forward n * delta"
  (setf (ip-location ip)
        (vector-+ 
          (ip-location ip)
          (vector-times-int (ip-delta ip) (pop-stack ip))))
  ip)

(define-funge-instruction #\x
  "Pop a vector, and set delta to that vector"
  (setf (ip-delta ip) (pop-vector ip))
  ip)

;;; Stack manipulation
(define-funge-instruction #\$
  "Pop and discard the top element of the stack"
  (pop-stack ip)
  ip)

(define-funge-instruction #\:
  "Duplicate the top stack element, pushing a copy of it onto the stack"
  (push (car (top-stack ip))
        (top-stack ip))
  ip)

(define-funge-instruction #\\
  "Swap the top two stack elements"
  (let ((a (pop-stack ip))
        (b (pop-stack ip)))
    (push a (top-stack ip))
    (push b (top-stack ip))
    ip))

(define-funge-instruction #\n
  "Clear the stack"
  (setf (top-stack ip)
        ())
  ip)

;;; Stack-stack manipulation
(define-funge-instruction #\{
  "Pop a value n from the stack, then push a new stack onto the stack-stack,
   and move n values from the old stack to the new stack. In addition, the
   storage offset is pushed to the old stack, and then set to LOCATION +
   DELTA"
  (let ((move-n (pop-stack ip)))
    (cond
      ((plusp move-n)
       (push (subseq (top-stack ip) 0 move-n)
             (ip-stack-stack ip))
       (setf (second (ip-stack-stack ip))
             (subseq (second (ip-stack-stack ip))
                     move-n)))
      ((zerop move-n)
       (push () (ip-stack-stack ip)))
      ((minusp move-n)
       (loop repeat (abs move-n) do
             (push 0 (top-stack ip)))
       (push () (ip-stack-stack ip))))) 
  (map nil (lambda (x)
             (push x (second (ip-stack-stack ip))))
       (ip-storage-offset ip))
  (setf (ip-storage-offset ip)
        (vector-+ (ip-location ip)
                  (ip-delta ip)))
  ip)

(define-funge-instruction #\}
  "Pop a vector from the second stack, and set the storage offset to that
   value. Then, pop a value n from the stack, and set the top n elements
   of the second stack to the top n values of the top stack.
   If there is only one stack on the stack-stack, act like `r'"
  (cond
    ((null (cdr (ip-stack-stack ip)))
     (funcall (gethash #\r *funge-98-instructions*)
              ip f-space))
    (t
     (setf (ip-storage-offset ip)
           (let ((y (pop (second (ip-stack-stack ip))))
                 (x (pop (second (ip-stack-stack ip)))))
             (vector (if (null x) 0 x)
                     (if (null y) 0 y))))
     (let ((move-n (pop-stack ip)))
       (cond
         ((plusp move-n)
          (setf (second (ip-stack-stack ip))
                (append
                  (subseq (top-stack ip) 0 move-n)
                  (second (ip-stack-stack ip)))))
         ((minusp move-n)
          (setf (second (ip-stack-stack ip))
                (subseq (second (ip-stack-stack ip))
                        (abs move-n))))))
     (pop (ip-stack-stack ip))))
  ip)

(define-funge-instruction #\u
  "Pop a value `n' from the stack, and then transfer that many values from the
   second stack to the top stack. If there is no second stack, reflect. If n
   is negative, transfer |n| values from the top stack to the second stack. If
   n is zero, do nothing"
  (if (null (cdr (ip-stack-stack ip)))
    (funcall (gethash #\r *funge-98-instructions*)
             ip f-space)
    (let ((move-n (pop-stack ip)))
      (cond
        ((plusp move-n)
         (loop repeat move-n do
               (push (pop (second (ip-stack-stack ip)))
                     (top-stack ip))))
        ((minusp move-n)
         (loop repeat (abs move-n) do
               (push (pop-stack ip)
                     (second (ip-stack-stack ip))))))))
  ip)

;;; System information retrieval
(define-funge-instruction #\y
  "Retrieves various information about the interpreter and the underlying OS.
   See http://quadium.net/funge/spec98.html#Sysinfo for details"
  (flet ((make-bit-field ()
           (make-array '(8) :element-type 'bit))
         (set-nth-lsb (bit-vector n bit)
           (setf (elt bit-vector (- (length bit-vector) n 1))
                 bit))
         (int<-bit-vector (bits)
           (reduce (lambda (a b)
                     (+ (ash a 1) b))
                   bits))
         (implemented (instruction)
           (if (gethash instruction *funge-98-instructions*)
             1
             0)))
    ;; Third cell.
    ;; Handprint. Placeholder value for now
    (push 16792875 (top-stack ip))
    ;; Second cell.
    ;; Bytes per cell. In puffball, cells store integers, and in Common Lisp,
    ;; the integer type has no limit on magnitude. So, we report zero to mean
    ;; unlimited
    (push 0 (top-stack ip))
    ;; First cell.
    ;; Implementation of varius instructions, un-/buffered input
    (let ((cell1 (make-bit-field)))
      (set-nth-lsb cell1 0
        (implemented #\t))
      (set-nth-lsb cell1 1
        (implemented #\i))
      (set-nth-lsb cell1 2
        (implemented #\o))
      (set-nth-lsb cell1 3
        (implemented #\=))
      (set-nth-lsb cell1 4 0)
      (push (int<-bit-vector cell1) (top-stack ip))))
  ip)

;;; Conditionals
(define-funge-instruction #\!
  "Pop a value and push one it it's zero, and zero if it's non-zero"
  (push (if (zerop (pop-stack ip))
          1
          0)
        (top-stack ip))
  ip)

(define-funge-instruction #\`
  "Pop two values and push 1 if the second is larger than the first"
  (push (if (< (pop-stack ip)
               (pop-stack ip))
          1
          0)
        (top-stack ip))
  ip)

(define-funge-instruction #\_
  "Pop a value and go right if it's zero, or left if it's non-zero"
  (setf (ip-delta ip)
        (if (zerop (pop-stack ip))
          #( 1 0)
          #(-1 0)))
  ip)

(define-funge-instruction #\|
  "Pop a value and go down if it's zero, or up if it's non-zero"
  (setf (ip-delta ip)
        (if (zerop (pop-stack ip))
          #(0  1)
          #(0 -1)))
  ip)

;;; Output
(define-funge-instruction #\,
  "Pop the top value off the stack, and print it as a character"
  (princ (code-char (pop-stack ip)))
  ip)

(define-funge-instruction #\.
  "Pop the top value off the stack, and print it as a (decimal) integer"
  (princ (pop-stack ip))
  ip)

;;; Funge-space manipulation
(define-funge-instruction #\'
  "Push the next character in funge-space onto the stack, and jump over it"
  (setf (ip-location ip)
        (vector-+ (ip-location ip)
                  (ip-delta    ip)))
  (push (char-code (char-at-vector f-space (ip-location ip)))
        (top-stack ip))
  ip)

(define-funge-instruction #\g
  "Pop a vector, then push the value of the cell at that vector in funge-space"
  (push (char-code (char-at-vector f-space
                                   (vector-+
                                     (pop-vector ip)
                                     (ip-storage-offset ip))))
        (top-stack ip))
  ip)

(define-funge-instruction #\p
  "Pop a vector, then a value, and set the cell at that vector in funge-space
   to that value"
  (set-f-space-location f-space (vector-+
                                  (pop-vector ip)
                                  (ip-storage-offset ip))
                        (code-char (pop-stack ip)))
  ip)

(define-funge-instruction #\s
  "Pop a value off the stack, and store the character corresponding to it
   in the next cell in the path of the ip"
  (set-f-space-location f-space (vector-+ (ip-location ip)
                                          (ip-delta ip))
                        (code-char (pop-stack ip)))
  (move-ip ip))
