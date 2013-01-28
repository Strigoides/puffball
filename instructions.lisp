;;;; Defines the hash-table containing all standard instructions.
;;;; Each instruction is a function, which is passed a single
;;;; argument; the ip that is executing it

(in-package :puffball)

(defvar *funge-98-instructions* (make-hash-table)
  "Standard instruction set for Funge-98")

(defmacro define-funge-instruction (name &body body)
  "Defines a funge instruction corresponding to the character NAME.
   Instructions have one, implicit argument: IP, which is bound to the ip
   executing the instruction. The return value of the function is assigned to
   the ip that executed it, so destructively modifying the ip passed in is
   perfectly acceptable"
  `(setf (gethash ,name *funge-98-instructions*)
         (lambda (ip)
           ,@body)))

;;; Instructions "0" through "9"
;;; The more natural DOTIMES or LOOP doesn't work here, as it closes over the
;;; same integer 10 times, and causes all 10 instructions to push 10 onto
;;; the stack
(mapc
  (lambda (n)
    (setf (gethash (digit-char n) *funge-98-instructions*) 
          (lambda (ip)
            (push n (car (ip-stack-stack ip)))
            ip)))
  (loop for x from 0 to 9 collecting x))

;;; Arithmetic
(define-funge-instruction #\+
  "Pop the top two stack values and add them together"
  (push (+ (pop (car (ip-stack-stack ip)))
           (pop (car (ip-stack-stack ip))))
        (car (ip-stack-stack ip)))
  ip)

(define-funge-instruction #\-
  "Pop the top two stack values and subtract the first from the second"
  (push (- (- (pop (car (ip-stack-stack ip)))
              (pop (car (ip-stack-stack ip)))))
        (car (ip-stack-stack ip)))
  ip)

(define-funge-instruction #\*
  "Pop the top two stack values and multiply them together"
  (push (* (pop (car (ip-stack-stack ip)))
           (pop (car (ip-stack-stack ip))))
        (car (ip-stack-stack ip)))
  ip)

(define-funge-instruction #\/
  "Pop the top two stack values and divide the second by the first"
  (let ((a (pop (car (ip-stack-stack ip))))
        (b (pop (car (ip-stack-stack ip)))))
    (push (floor b a) (car (ip-stack-stack ip)))
    ip))

(define-funge-instruction #\%
  "Pop the top two stack values and find the remainder of dividing the second
   by the first"
  (let ((a (pop (car (ip-stack-stack ip))))
        (b (pop (car (ip-stack-stack ip)))))
    (push (mod b a) (car (ip-stack-stack ip)))
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

(define-funge-instruction #\,
  "Pop the top value off the stack, and print it as a character"
  (princ (pop (car (ip-stack-stack ip))))
  ip)

(define-funge-instruction #\#
  "`Tramponline' instruction; jump over one cell"
  (setf (ip-location ip)
        (vector-+ (ip-location ip)
                  (ip-delta ip)))
  ip)

(define-funge-instruction #\r
  "Reverse the direction of travel"
  (setf (ip-delta ip)
        (map 'vector
             (lambda (x) (* x -1))
             (ip-delta ip)))
  ip)
