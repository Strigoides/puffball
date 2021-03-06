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
         (instruction-lambda ',body)))

(defun instruction-lambda (body)
  "Given a body of code, create a lambda function which assumes that the code
   is the body of a funge instruction; i.e. it receives IP and F-SPACE
   arguments, and F-SPACE is declared ignorable as it's unused most of the
   time"
  (eval
    `(lambda (ip f-space)
       ,@(if (stringp (car body)) ; Put docstrings first
           `(,(car body)
              (declare (ignorable f-space))
              ,@(cdr body))
           `((declare (ignorable f-space))
             ,@body)))))

(defun call-funge-instruction (char ip f-space)
  "Call the instruction corresponding to CHAR with args IP and F-SPACE, and
   return the value returned by that instruction"
  (funcall (gethash char *funge-98-instructions*)
           ip f-space))

;;; Instructions "0" to "9", and "a" to "f"
;;; The more natural DOTIMES or LOOP doesn't work here, as it closes over the
;;; same integer 16 times, and causes all 16 instructions to push 16 onto
;;; the stack
(map nil
     (lambda (n char)
       (setf (gethash char *funge-98-instructions*) 
             (lambda (ip f-space)
               (declare (ignore f-space))
               (push n (top-stack ip))
               ip)))
     (loop for x from 0 to 15 collecting x)
     "0123456789abcdefg")

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

(define-funge-instruction #\z
  "NOP"
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

(define-funge-instruction #\?
  "Head in a random direction"
  (setf (ip-delta ip)
        (elt '(#(-1  0)
               #( 1  0)
               #( 0 -1)
               #( 0  1))
             (random 4)))
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
    (call-funge-instruction (cond
                           ((> a b) #\[)
                           ((< a b) #\])
                           (t #\z))
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
  (let ((duplicate-me (pop-stack ip)))
    (loop repeat 2 do
          (push duplicate-me (top-stack ip))))
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
     (call-funge-instruction #\r ip f-space))
    (t
     (setf (ip-storage-offset ip)
           (let ((y (pop (second (ip-stack-stack ip))))
                 (x (pop (second (ip-stack-stack ip)))))
             (vector (or x 0)
                     (or y 0))))
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
    (call-funge-instruction #\r ip f-space)
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
  (move-ip ip)
  (push (char-code (char-at-vector f-space (ip-location ip)))
        (top-stack ip))
  ip)

(define-funge-instruction #\"
  "Switch to string mode; any characters encountered until the next double
   quote are pushed as numeric values onto the stack. Multiple spaces in a row
   are ignored, and only one space is pushed onto the stack"
  (declare (optimize (debug 3)))
  (move-ip ip)
  (flet ((wrap (vector)
           (apply #'wrap vector (f-space-size f-space))))
    (let ((delta (ip-delta ip)))
      (do ((location (wrap (ip-location ip))
                     (if (char= (char-at-vector f-space location) #\Space)
                       (do ((location2 (vector-+ location delta)
                                       (wrap (vector-+ location2 delta))))
                         ((char/= (char-at-vector f-space location2)
                                  #\Space)
                          location2))
                       (wrap (vector-+ location
                                       (ip-delta ip))))))
        ((char= (char-at-vector f-space location) #\")
         (setf (ip-location ip) location))
        (push (char-code (char-at-vector f-space location))
              (top-stack ip)))
      ip)))

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

;;; Fingerprints
;;; The #+nil #\) is there because #\( screws up slimv's parenthesis balancing
(define-funge-instruction #\( #+nil #\)
  "Pop a value n, then pop n values, turn them into a fingerprint id, and load
   the fingerprint associated with that id. If the fingerprint is not found,
   reflect"
  ;;; TODO: Save overwritten values and restore when unloading semantics
  (let ((fingerprint (get-fingerprint-from-char-codes
                       (loop repeat (pop-stack ip) collecting
                             (pop-stack ip)))))
    (if fingerprint
      (loop for char being the hash-keys in fingerprint
            using (hash-value instruction) do
            (setf (gethash char *funge-98-instructions*)
                  instruction))
      (call-funge-instruction #\r ip f-space))))

(define-funge-instruction #\)
  "Pop a value n, then pop n values, turn them into a fingerprint id, and
   unload all semantics associated with that fingerprint"
  (let ((fingerprint (get-fingerprint-from-char-codes
                       (loop repeat (pop-stack ip) collecting
                             (pop-stack ip)))))
    (if fingerprint
      (loop for char being the hash-keys in fingerprint do
            (remhash char *funge-98-instructions*))
      (call-funge-instruction #\r ip f-space))))
