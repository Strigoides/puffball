;;;; Funge-space
(in-package :puffball)

(defconstant +default-size+ '(1000 1000)
             "By default, f-space objects have these dimensions")

(defstruct f-space
  ;; TODO: Make this able to be dynamically resized, rather than just
  ;; making it big enough for most cases
  ;; TODO: Generalize to deal with Unefunge and Trefunge
  (positive-quadrant (make-array +default-size+
                                 :element-type 'character
                                 :initial-element #\Space)
                     :type (simple-array character))
  (negative-quadrants (make-hash-table :test #'equal)
                      :type hash-table)
  (actual-width  0 :type integer) ; The array used for funge-space is usually
  (actual-height 0 :type integer)); larger than funge-space itself, to allow
                                  ; extending easily, but we need to store the
                                  ; actual dimensions for wraparound and stuff

(defun f-space-size (f-space)
  (list (slot-value f-space 'actual-width)
        (slot-value f-space 'actual-height)))

(defun char-at-vector (f-space vector)
  "Return the character at the location given by a vector in the form: #(X Y)"
  (char-at f-space (elt vector 0) (elt vector 1)))

(defun char-at (f-space x y)
  "Return the character at the location given by two ints"
  (if (or (minusp x)
          (minusp y))
    (or (gethash (list x y) (slot-value f-space 'negative-quadrants))
        #\Space) 
    (aref (slot-value f-space 'positive-quadrant) x y)))

(defun set-f-space-location (f-space vector char)
  "Set the location in F-SPACE given by VECTOR to CHAR"
  (if (or (minusp (elt vector 0)) 
          (minusp (elt vector 1)))
    (setf (gethash (coerce vector 'list)
                   (slot-value f-space 'negative-quadrants))
          char)
    (setf (aref (slot-value f-space 'positive-quadrant)
                (elt vector 0) (elt vector 1))
          char)))

(defun load-f-space (code-string)
  "Create an f-space object corresponding to the funge program in CODE-STRING"
  (let ((f-space (make-f-space)))
    (loop for char across code-string
          with x = 0 maximizing x into longest-line
          with y = 0
          until (char= char #\Nul)
          do (if (char= char #\Newline)
               (setf x 0
                     y (1+ y))
               (progn
                 (set-f-space-location f-space (vector x y) char)
                 (incf x)))
          finally
            (setf (f-space-actual-width f-space)
                  (1+ longest-line))
            (setf (f-space-actual-height f-space)
                  (1+ y))
            (return f-space))))

(defun print-f-space-region (f-space x1 y1 x2 y2)
  "Print the rectangle with top-left corner at (x1, y1), and bottom-right at
   (x2, y2) from the given f-space object"
  (loop for y from y1 to y2 do
        (loop for x from x1 to x2 do
              (princ (char-at f-space x y))) 
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
