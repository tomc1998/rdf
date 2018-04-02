(ql:quickload :prove)
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)
(ql:quickload :parenscript)

(defpackage :rdf
  (:use  "PARENSCRIPT" "COMMON-LISP")
  (:export :rdf-start
           :rdf-stop
           :define-app-req
           :register-component
           :set-view-routes
           :*server-ref*))

(defpackage :rdf-full-example
  (:use "COMMON-LISP")
  (:export :main))

(asdf:defsystem rdf
  :description "A rapid development web framework"
  :depends-on (:corm :hunchentoot :cl-json :parenscript)
  :components ((:file "main/json-ser")
               (:file "main/json-deser")
               (:file "main/view/view")
               (:file "main/main"
                      :depends-on ("main/json-ser" "main/json-deser" "main/view/view")
                      )))

(asdf:defsystem rdf-tests
  :depends-on (:rdf :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main")
               (:test-file "test/json")))

(asdf:defsystem rdf-full-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf)
  :components ((:file "examples/full/main")))
