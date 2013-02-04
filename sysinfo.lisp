;;;; System information retrieval
(in-package :puffball)

(defvar *info-funs*
  (make-array '(20) :element-type 'function)
  "Functions for getting system information. The function for getting
   info slot `n' is element `n' in this array")

(defmacro definfo (n &body body)
  "Define the function for getting info slot n. Information slots are indexed
   starting with 1, as per the list in the spec. Should return either an int or
   a list. Each function takes an ip, and an f-space object."
  `(setf (elt *info-funs* ,(1- n))
         (lambda (ip f-space)
           ,@body)))

(defun values<-vector (vector)
  "Given a vector, return a list of values to be pushed onto a stack, such that
   popping them off in the conventional funge-98 way will pop that vector"
  (coerce vector 'list))

(definfo 1
  "Implementation status of various instructions, and un-/buffered input"
  (declare (ignore ip f-space))
  (flet ((set-nth-lsb (bit-vector n bit)
           (setf (elt bit-vector (- (length bit-vector) n 1))
                 bit))
         (implemented (instruction)
           (if (gethash instruction *funge-98-instructions*)
             1
             0)))
    (let ((cell1 (make-array '(8) :element-type 'bit)))
      (set-nth-lsb cell1 0 (implemented #\t))
      (set-nth-lsb cell1 1 (implemented #\i))
      (set-nth-lsb cell1 2 (implemented #\o))
      (set-nth-lsb cell1 3 (implemented #\=))
      (set-nth-lsb cell1 4 0)
      (reduce (lambda (a b)
                (+ (ash a 1) b))
              cell1))))

(definfo 2
  "Bytes per cell. In puffball, cells store integers, and in Common Lisp, the
   integer type has no limit on magnitude. So, we report zero to mean unlimited"
  (declare (ignore ip f-space))
  0)

(definfo 3
  "Handprint. Placeholder value for now" 
  (declare (ignore ip f-space))
  16792875)

(definfo 4
  "Version number. Stick with 0 for now."
  (declare (ignore ip f-space))
  0) 

(definfo 5
  "What should = behave like? If we're on a unix-like OS, it's system()-like,
   otherwise, default to unavailable (once it's actually implemented that is)"
  (declare (ignore ip f-space))
  #|
  #+unix 1
  #-unix 0
  |#
  0)

(definfo 6
  "Path seperator. / if unix-like, \ if windows"
  (declare (ignore ip f-space))
  (char-code
    #+windows #\\   
    #+unix #\/))

(definfo 7
  "Number of dimensions. Only befunge is supported so far, so return 2"
  (declare (ignore ip f-space))
  2)

(definfo 8
  "ID for the current IP. Concurrency is not supported yet, so arbitrarily
   return 0"
  (declare (ignore ip f-space))
  0)

(definfo 9
  "Team number. I don't even know what this is. Push 0 arbitrarily"
  (declare (ignore ip f-space))
  0)

(definfo 10
  "Location of the current IP"
  (declare (ignore f-space))
  (values<-vector (ip-location ip)))

(definfo 11
  "Delta of the current IP"
  (declare (ignore f-space))
  (values<-vector (ip-delta ip))) 

(definfo 12
  "Storage offset of the current IP"
  (declare (ignore f-space))
  (values<-vector (ip-storage-offset ip)))

(defun find-min-point (f-space)
  "Find the lowest x and y values which are occupied"
  (loop for point being the hash-keys in
        (f-space-negative-quadrants f-space)
        minimizing (elt point 0) into min-x
        minimizing (elt point 1) into min-y
        finally (return (vector min-x min-y))))

(definfo 13
  "Upper left bouding point of funge-space"
  (declare (ignore ip))
  (values<-vector (find-min-point f-space))) 

(definfo 14
  "Lower right bounding point of funge-space, relative to upper left point"
  (declare (ignore ip))
  (values<-vector
    (vector-minus (vector (f-space-actual-width f-space)
                          (f-space-actual-height f-space))
                  (find-min-point f-space)))) 

(definfo 15
  "Current date"
  (declare (ignore ip f-space))
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (+ (* (- year 1900) 256 256)
       (* month 256) 
       date)))

(definfo 16
  "Current time"
  (declare (ignore ip f-space))
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (+ (* hour 256 256)
       (* minute 256)
       second)))

(definfo 17
  "Size of stack-stack"
  (declare (ignore f-space))
  (length (ip-stack-stack ip)))

(definfo 18
  "Size of each stack"
  (declare (ignore f-space))
  (mapcar #'length (ip-stack-stack ip)))

(defun null-delimited-ints<-string-list (strings)
  (let ((funge-string-list ()))
    (dolist (string strings)
      (loop for char across string do
            (push (char-code char) funge-string-list))
      (push 0 funge-string-list)) 
    (push 0 funge-string-list)
    funge-string-list))

(definfo 19
  "argv. There is no standard way to fetch command line arguments.
   I might extend this for other implmentations later, but for now let's
   just use the sbcl extension" 
  (declare (ignore ip f-space))
  (cons 0 ; argv has an additional null on the end 
        (null-delimited-ints<-string-list 
          (cdr (or #+sbcl sb-ext:*posix-argv* nil))))) 

(definfo 20
  "Environmental variables. Uses the sbcl extension"
  (declare (ignore ip f-space))
  (null-delimited-ints<-string-list
    (sb-unix::posix-environ))) 

(define-funge-instruction #\y
  "Retrieves various information about the interpreter and the underlying OS.
   See http://quadium.net/funge/spec98.html#Sysinfo for details"
  (let ((info-select (pop-stack ip)))
    ;; If info-select is larger than 20, don't even bother pushing info
    (flet ((push-datum (datum)
             (if (listp datum)
               (dolist (subdatum datum)
                 (push subdatum (top-stack ip)))
               (push datum (top-stack ip))))) 
      (if (plusp info-select)
        (loop for i from 0
          for current-info = (funcall (elt *info-funs* i) ip f-space)
          for info-length = (if (listp current-info)
                              (length current-info)
                              1)
          summing info-length into cell-count
          when (>= cell-count info-select)
            do (push (if (listp current-info)
                       (car (last current-info (- cell-count info-length)))
                       current-info)
                     (top-stack ip))
            and do (return)
          when (= i 19)
            do (push (elt (top-stack ip)
                          (- info-select cell-count))
                     (top-stack ip))
            and do (return))

        (mapcar #'push-datum
          (map 'list (lambda (info-fun)
                       (funcall info-fun ip f-space))
               (reverse *info-funs*))))))
  ip)
