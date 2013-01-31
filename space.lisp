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

(defun char-at-vector (f-space vector)
  "Return the character at the location given by a vector in the form: #(X Y)"
  (char-at f-space (elt vector 0) (elt vector 1)))

(defun char-at (f-space x y)
  "Return the character at the location given by two ints"
  (aref f-space x y))

(defun load-f-space (code-string)
  "Create an f-space object corresponding to the funge program in CODE-STRING"
  (let ((f-space (make-f-space 1000 1000)))
    (loop for char across code-string
          with x = 0
          with y = 0
          until (char= char #\Nul)
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

(defun wrap (vector width height)
  "Wrap a location vector, assuming that funge-space has dimensions WIDTHxHEIGHT"
  ;; FIXME: Breaks on vectors with x/y more than twice width/height off the edge
  ;; TODO: Implement parser for wrapping functions as defined at:
  ;;       http://quadium.net/funge/spec98.html#Topologies
  ;; TODO: Proper backtrack wrapping, rather than toroidal
  (flet ((wrap-value (value max)
           (cond
             ((minusp value)
              (+ max value))
             ((>= value max)
              (- value max))
             (t value))))
    (vector (wrap-value (elt vector 0) width)
            (wrap-value (elt vector 1) height))))
