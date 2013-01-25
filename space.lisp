;;;; Funge-space
(in-package :puffball)

(defun make-f-space (width height)
  "Return a new funge-space object."
  ;; TODO: Make this able to be dynamically resized, rather than just
  ;; making it big enough for most cases
  ;; TODO: Generalize to deal with Unefunge and Trefunge
  (make-array (list width height)
              :element-type 'character
              :initial-element #\Space))
