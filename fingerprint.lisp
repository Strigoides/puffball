;;;; Code for defining fingerprints
(in-package :puffball)

(defvar *fingerprints* (make-hash-table)
  "Hash table holding fingerprints. Indexed by the ID of the fingerprint,
   and containing hash tables containing the semantics for that fingerprint.")

(defmacro deffingerprint (name &optional docstring)
  "Define a new fingerprint, creating a hash table for it in *FINGERPRINTS*.
   NAME can be a symbol or a string; either way it's converted to a string.
   There isn't really any way to attach the docstring to the hash table, so
   it's purely for the reader of the source."
  (declare (ignore docstring))
  `(setf (gethash (id<-fp-string ,(string name)) *fingerprints*)
         (make-hash-table)))

(defun id<-fp-string (fingerprint-name)
  "Take the string name of a fingerprint (such as ''NULL'') and turns it into
   the standard ID used to lookup fingerprints"
  (id<-char-codes (map 'list #'char-code fingerprint-name)))

(defun id<-char-codes (char-codes)
  "Take a list of char-codes designating a fingerprint, and return the ID
   used to lookup that fingerprint"
  (loop for code in char-codes
        with id = 0 do
        (setf id (ash id 8))
        (incf id code)
        finally (return id)))
