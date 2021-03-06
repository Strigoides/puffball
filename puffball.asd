(asdf:defsystem puffball
  :description "A Funge-98 interpreter"
  :components ((:file "packages")
               (:file "fingerprint"
                :depends-on ("packages"))
               (:module fingerprints
                        :pathname "fingerprints"
                        :components ((:file "NULL"))
                        :depends-on ("packages" "fingerprint"))
               (:file "sysinfo"
                :depends-on ("packages" "instructions"))
               (:file "vector"
                :depends-on ("packages"))
               (:file "instructions"
                :depends-on ("packages" "ip"))
               (:file "interpreter"
                :depends-on ("packages" "ip"))
               (:file "space"
                :depends-on ("packages"))
               (:file "ip"
                :depends-on ("packages"))))
