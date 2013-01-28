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
