;;;; Instruction pointer
(in-package :puffball)

(defstruct ip
  "An instruction pointer. Represents the travel of control flow around
   funge-space. Each instruction pointer has its own location, delta,
   and stack-stack"
  (location       #(0 0)    :type (simple-vector 2))
  (delta          #(1 0)    :type (simple-vector 2))
  (stack-stack    (list ()) :type list)
  (storage-offset #(0 0)    :type (simple-vector 2)))

(defmacro top-stack (ip)
  `(car (ip-stack-stack ,ip)))

(defun move-ip (ip)
  "Move the given ip by its delta, and return the ip"
  (setf (ip-location ip)
        (vector-+ (ip-location ip)
                  (ip-delta ip)))
  ip)

(defun pop-stack (ip)
  "Pop the top value off the stack and return it. When the stack is empty,
   return zero instead"
  (or (pop (top-stack ip))
      0))

(defun pop-vector (ip)
  "Pop a vector off the stack"
  (let ((y (pop-stack ip))
        (x (pop-stack ip)))
    (vector x y)))

(defun next-instruction (start delta f-space)
  "Starting from START, moving by DELTA, find the first cell that is not a
   space, also excluding semicolon delimited blocks"
  (destructuring-bind (width height) (f-space-size f-space)
    (do ((location start
                   (case (char-at-vector f-space location)
                     (#\Space (wrap (vector-+ location delta)
                                    width height))
                     (#\; (do ((location2 (vector-+ location delta)
                                          (vector-+ location2 delta)))
                            ((char= (char-at-vector f-space location2)
                                    #\;)
                             (vector-+ location2 delta)))))))
      ((not (member (char-at-vector f-space location)
                    '(#\Space #\;)))
       location))))
