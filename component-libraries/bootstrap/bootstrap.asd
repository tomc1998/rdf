(defpackage :bs
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :load-all
           :bs-container
           :bs-row
           :bs-col))

(asdf:defsystem bootstrap
  :description "A bootstrap component library for rdf"
  :depends-on (:rdf)
  :components ((:file "main")))
