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
             0))
         (push-vector (vector ip)
           (push (elt vector 0) (top-stack ip))
           (push (elt vector 1) (top-stack ip)))
         (push-string-list (strings)
           (push 0 (top-stack ip))
           (dolist (string (reverse strings))
             (push 0 (top-stack ip))
             (loop for char across (reverse string) do
                   (push (char-code char)
                         (top-stack ip))))))

    ;; y returns the size of each stack *before* any information is pushed on.
    ;; So, we need to measure this now and then keep it until 18.
    (let ((stack-sizes (reverse (mapcar #'length (ip-stack-stack ip)))))
      ;; 20.
      ;; Environmental variables. Uses the sbcl extension
      (push-string-list (sb-unix::posix-environ)) 
      ;; 19. 
      ;; argv. There is no standard way to fetch command line arguments.
      ;; I might extend this for other implmentations later, but for now let's
      ;; just use the sbcl extension
      (push-string-list
        (cdr (or #+sbcl sb-ext:*posix-argv* nil))) 
      (push 0 (top-stack ip)) ; argv has an additional null on the end
      ;; 18.
      ;; Size of each stack
      (dolist (size stack-sizes)
        (push size (top-stack ip)))
    ;; 17.
    ;; Size of stack-stack
    (push (length (ip-stack-stack ip))
          (top-stack ip))
    ;; 16.
    ;; Current time
    (multiple-value-bind
      (second minute hour date month year) (get-decoded-time)
      (push (+ (* hour 256 256)
               (* minute 256)
               second)
            (top-stack ip))
      ;; 15.
      ;; Current date
      (push (+ (* (- year 1900) 256 256)
               (* month 256) 
               date)
            (top-stack ip))) 
    ;; Work minimum cell out now, as it's needed for 13. and 14.
    (let ((min-point
            (loop for point being the hash-keys in
                  (f-space-negative-quadrants f-space)
                  minimizing (elt point 0) into min-x
                  minimizing (elt point 1) into min-y
                  finally (return (vector min-x min-y)))))
      ;; 14.
      ;; Lower right bounding point of funge-space, relative to upper left point
      (push-vector (vector-minus (vector (f-space-actual-width f-space)
                                         (f-space-actual-height f-space))
                                 min-point)
                   ip) 
      ;; 13.
      ;; Upper left bouding point of funge-space
      (push-vector min-point ip)) 
    ;; 12.
    ;; Storage offset of the current IP
    (push-vector (ip-storage-offset ip) ip)
    ;; 11.
    ;; Delta of the current IP
    (push-vector (ip-delta ip) ip) 
    ;; 10. 
    ;; Location of the current IP
    (push-vector (ip-location ip) ip)
    ;; 9. 
    ;; Team number. I don't even know what this is. Push 0 arbitrarily
    (push 0 (top-stack ip))
    ;; 8. 
    ;; ID for the current IP. Concurrency is not supported yet, so arbitrarily
    ;; push 0
    (push 0 (top-stack ip))
    ;; 7. 
    ;; Number of dimensions. Only befunge is supported so far, so just push 2
    (push 2 (top-stack ip))
    ;; 6. 
    ;; Path seperator. / if unix-like, \ if windows
    (push (char-code
            #+unix #\/
            #+windows #\\ )
          (top-stack ip))
    ;; 5. 
    ;; What should = behave like? If we're on a unix-like OS, it's
    ;; system()-like, otherwise, default to unavailable
    #+unix (push 1 (top-stack ip))
    #-unix (push 0 (top-stack ip))
    ;; 4. 
    ;; Version number. Stick with 0 for now.
    (push 0 (top-stack ip))
    ;; 3. 
    ;; Handprint. Placeholder value for now
    (push 16792875 (top-stack ip))
    ;; 2. 
    ;; Bytes per cell. In puffball, cells store integers, and in Common Lisp,
    ;; the integer type has no limit on magnitude. So, we report zero to mean
    ;; unlimited
    (push 0 (top-stack ip))
    ;; 1.
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
      (push (int<-bit-vector cell1) (top-stack ip))))) 
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
