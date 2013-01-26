;;;; Running this with sbcl --script puffball.lisp [filename] runs the
;;;; befunge program in the file [filename]

(require 'asdf)
(asdf:load-system :puffball)

(in-package :puffball)
(run-file (cadr sb-ext:*posix-argv*))
