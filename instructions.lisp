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
        (map 'vector
             (lambda (x) (* x -1))
             (ip-delta ip)))
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
  (push (char-code (char-at-vector f-space (pop-vector ip)))
        (top-stack ip))
  ip)

(define-funge-instruction #\p
  "Pop a vector, then a value, and set the cell at that vector in funge-space
   to that value"
  (set-f-space-location f-space (pop-vector ip) (code-char (pop-stack ip)))
  ip)
