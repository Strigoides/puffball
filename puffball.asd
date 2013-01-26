(asdf:defsystem puffball
  :description "A Funge-98 interpreter"
  :components ((:file "packages")
               (:file "instructions"
                :depends-on ("packages"))
               (:file "interpreter"
                :depends-on ("packages"))
               (:file "space"
                :depends-on ("packages"))
               (:file "ip"
                :depends-on ("packages"))))
