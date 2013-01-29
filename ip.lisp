;;;; Instruction pointer
(in-package :puffball)

(defstruct ip
  "An instruction pointer. Represents the travel of control flow around
   funge-space. Each instruction pointer has its own location, delta,
   and stack-stack"
  (location    #(0 0)             :type (simple-vector 2))
  (delta       #(1 0)             :type (simple-vector 2))
  (stack-stack (list ())  :type list))

(defmacro top-stack (ip)
  `(car (ip-stack-stack ,ip)))

(defun pop-stack (ip)
  "Pop the top value off the stack and return it. When the stack is empty,
   return zero instead"
  (or (pop (top-stack ip))
      0))

(defun pop-vector (ip)
  "Pop a vector off the stack"
  (let ((x (pop-stack ip))
        (y (pop-stack ip)))
    (vector x y)))
