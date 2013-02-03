;;;; System information retrieval

(defvar *info-funs*
  (make-array '(20) :element-type 'function)
  "Functions for getting system information. The function for getting
   info slot `n' is element `n' in this array")

(defmacro definfo (n &body body)
  "Define the function for getting info slot n. Information slots are indexed
   starting with 1, as per the list in the spec. Should return either an int or
   a list. Each function takes an ip, and an f-space."
  `(setf (nth ,n *info-funs*)
         (lambda (ip f-space)
           ,@body)))

(define-funge-instruction #\y
  "Retrieves various information about the interpreter and the underlying OS.
   See http://quadium.net/funge/spec98.html#Sysinfo for details"
  (let ((info-select (pop-stack ip))
        (info ()))
    ;; If info-select is larger than 20, don't even bother pushing info
    (if (> info-select 20)
      (push (elt (top-stack ip) (- info-select 21))
            (top-stack ip))
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
             (push-vector (vector)
               (let ((vector-list ()))
                 (push (elt vector 0) vector-list) 
                 (push (elt vector 1) vector-list)
                 (push vector-list info)))
             (push-string-list (strings)
               (let ((funge-string-list ()))
                 (dolist (string strings)
                   (loop for char across string do
                         (push (char-code char) funge-string-list))
                   (push 0 funge-string-list)) 
                 (push 0 funge-string-list)
                 (push funge-string-list info))))
        ;; 1. Implementation of varius instructions, un-/buffered input
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
          (push (int<-bit-vector cell1) info))
        ;; 2. Bytes per cell. In puffball, cells store integers, and in Common
        ;; Lisp, the integer type has no limit on magnitude. So, we report zero to
        ;; mean unlimited
        (push 0 info)
        ;; 3. Handprint. Placeholder value for now
        (push 16792875 info)
        ;; 4. Version number. Stick with 0 for now.
        (push 0 info)
        ;; 5. What should = behave like? If we're on a unix-like OS, it's
        ;; system()-like, otherwise, default to unavailable
        (push #+unix 1
              #-unix 0
              info)
        ;; 6. Path seperator. / if unix-like, \ if windows
        (push (char-code
                #+unix #\/
                #+windows #\\ )
              info)
        ;; 7. Number of dimensions. Only befunge is supported so far, so push 2
        (push 2 info)
        ;; 8. ID for the current IP. Concurrency is not supported yet, so
        ;; arbitrarily push 0
        (push 0 info)
        ;; 9. Team number. I don't even know what this is. Push 0 arbitrarily
        (push 0 info)
        ;; 10. Location of the current IP
        (push-vector (ip-location ip))
        ;; 11. Delta of the current IP
        (push-vector (ip-delta ip)) 
        ;; 12. Storage offset of the current IP
        (push-vector (ip-storage-offset ip))
        ;; Work minimum cell out now, as it's needed for 13. and 14.
        (let ((min-point
                (loop for point being the hash-keys in
                      (f-space-negative-quadrants f-space)
                      minimizing (elt point 0) into min-x
                      minimizing (elt point 1) into min-y
                      finally (return (vector min-x min-y)))))
          ;; 13. Upper left bouding point of funge-space
          (push-vector min-point) 
          ;; 14. Lower right bounding point of funge-space, relative to upper
          ;; left point
          (push-vector (vector-minus (vector (f-space-actual-width f-space)
                                             (f-space-actual-height f-space))
                                     min-point))) 
        (multiple-value-bind
          (second minute hour date month year) (get-decoded-time)
          ;; 15. Current date
          (push (+ (* (- year 1900) 256 256)
                   (* month 256) 
                   date)
                info) 
          ;; 16. Current time
          (push (+ (* hour 256 256)
                   (* minute 256)
                   second)
                info)) 
        ;; 17. Size of stack-stack
        (push (length (ip-stack-stack ip)) info)
        ;; 18. Size of each stack
        (push (mapcar #'length (ip-stack-stack ip)) info)
        ;; 19. argv. There is no standard way to fetch command line arguments.
        ;; I might extend this for other implmentations later, but for now let's
        ;; just use the sbcl extension
        (push-string-list
          (cdr (or #+sbcl sb-ext:*posix-argv* nil))) 
        (push 0 info) ; argv has an additional null on the end
        ;; 20. Environmental variables. Uses the sbcl extension
        (push-string-list (sb-unix::posix-environ)) 

        (flet ((push-datum (datum)
                 (if (listp datum)
                   (dolist (subdatum datum)
                     (push subdatum (top-stack ip)))
                   (push datum (top-stack ip))))) 
          (if (plusp info-select)
            (push-datum (nth info-select info))
            (dolist (datum info)
              (push-datum datum)))))))
  ip)
