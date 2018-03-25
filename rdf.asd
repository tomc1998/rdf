(ql:quickload :prove)
(ql:quickload :hunchentoot)

(defpackage :rdf
  (:use "COMMON-LISP")
  (:export :rdf-start
           :rdf-stop
           :app-req
           :*server-ref*))

(defpackage :rdf-full-example
  (:use "COMMON-LISP")
  (:export :main))

(asdf:defsystem rdf
  :description "A rapid development web framework"
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:corm :hunchentoot)
  :components ((:file "main/main")))

(asdf:defsystem tests
  :depends-on (:rdf :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main"))
  )

(asdf:defsystem rdf-full-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf)
  :components ((:file "examples/full/main")))
