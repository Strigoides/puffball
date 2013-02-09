;;;; Main interpreter functions

(in-package :puffball)

(defun run (funge-code-string instructions)
  "Run FUNGE-CODE-STRING as a Funge-98 program. INSTRUCTIONS contains a hash,
   mapping characters to instructions"
  ;; TODO: Concurrency
  (let ((f-space     (load-f-space funge-code-string))
        (ip          (make-ip)))
    (loop while ip do
          (let ((current-char (char-at-vector f-space (ip-location ip))))
            (setf ip (funcall (or (gethash current-char instructions)
                                  (gethash #\r instructions))
                              ip f-space))
            (when ip
              (setf (ip-location ip)
                    (next-instruction (vector-+ (ip-location ip)
                                                (ip-delta ip))
                                      (ip-delta ip)
                                      f-space)))))))

(defun run-file (pathname &optional (instructions *funge-98-instructions*))
  "Run the file given by PATHNAME as a Funge-98 program"
  (run
    (with-open-file (stream pathname)
      (let ((out-string (make-string (file-length stream))))
        (read-sequence out-string stream)
        out-string))
    instructions))
