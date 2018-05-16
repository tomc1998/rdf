(asdf:defsystem bootstrap
  :description "A bootstrap component library for rdf"
  :depends-on (:rdf)
  :components ((:file "packages") (:file "main")))
