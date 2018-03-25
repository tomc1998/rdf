(ql:quickload :prove)
(ql:quickload :hunchentoot)

(defpackage :rdf
  (:use "COMMON-LISP")
  (:export :rdf-start
           :rdf-stop
           :*server-ref*)
  )
(in-package :rdf)

(asdf:defsystem rdf
  :description "An example HTTP server in lisp"
  :version "0.0.1"
  :author "Tom <thomascheng1998@gmail.com>"
  :licence "Public Domain"
  :depends-on (:corm :hunchentoot)
  :components ((:file "main/main")))

(asdf:defsystem tests
  :depends-on (:rdf :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main"))
  )
