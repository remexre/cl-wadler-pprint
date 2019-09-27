(defsystem #:cl-wadler-pprint
  :description "An implementation of A Prettier Printer in Common Lisp."
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :version "0.1.0"
  :serial t
  :depends-on ()
  :in-order-to ((test-op (test-op #:cl-wadler-pprint/test)))
  :components ((:file "package")
               (:file "wadler-pprint")))

(defsystem #:cl-wadler-pprint/test
  :description "Tests for cl-wadler-pprint."
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-wadler-pprint #:fiveam)
  :perform (test-op (o c)
             (uiop:symbol-call '#:fiveam '#:run!
                (uiop:find-symbol* '#:wadler-pprint-suite '#:wadler-pprint-tests)))
  :components ((:file "test")))

; vi: ft=lisp :
