(asdf:defsystem puffball
  :description "A Funge-98 interpreter"
  :components ((:file "packages")
               (:file "space"
                :depends-on ("packages"))))
