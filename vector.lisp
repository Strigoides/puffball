;;;; A few handy functions for integer vectors

(in-package :puffball)

(defun vector-+ (vector1 vector2)
  (map 'vector #'+ vector1 vector2))

(defun vector-minus (vector1 vector2)
  (map 'vector #'- vector1 vector2))
