(defpackage :bs
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :load-all :gen-form))

(asdf:defsystem bootstrap
  :description "A bootstrap component library for rdf"
  :depends-on (:rdf)
  :components ((:file "main")))
