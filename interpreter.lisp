;;;; Main interpreter functions

(in-package :puffball)

(defun run (funge-code-string instructions)
  "Run FUNGE-CODE-STRING as a Funge-98 program. INSTRUCTIONS contains a hash,
   mapping characters to instructions"
  ;; TODO: Concurrency
  (let ((f-space (load-f-space funge-code-string))
        (ip      (make-ip)))
    (loop while ip do
          (setf ip (funcall 
                     (gethash (char-at f-space (ip-location ip)) instructions)
                     ip))
          (when ip
            (incf (elt (ip-location ip) 0)
                  (elt (ip-delta ip)    0))
            (incf (elt (ip-location ip) 1)
                  (elt (ip-delta ip)    1))))))
