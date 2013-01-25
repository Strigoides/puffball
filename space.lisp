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

(defun load-f-space (code-string)
  "Create an f-space object corresponding to the funge program in CODE-STRING"
  (let ((f-space (make-f-space 1000 1000)))
    (loop for char across code-string
          with x = 0
          with y = 0
          do (if (char= char #\Newline)
               (setf x 0
                     y (1+ y))
               (progn
                 (setf (aref f-space x y) char)
                 (incf x)))

          finally (return f-space))))

(defun print-f-space-region (f-space x1 y1 x2 y2)
  "Print the rectangle with top-left corner at (x1, y1), and bottom-right at
   (x2, y2) from the given f-space object"
  (loop for y from y1 to y2 do
        (loop for x from x1 to x2 do
              (princ (aref f-space x y)))
        (princ #\Newline)))
