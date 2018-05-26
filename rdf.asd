(asdf:defsystem rdf
  :description "A rapid development web framework"
  :depends-on (:corm :hunchentoot :cl-json :parenscript :str :ironclad :flexi-streams :lass :cl-smtp)
  :components ((:file "packages")
               (:file "main/json-ser")
               (:file "main/json-deser")
               (:file "main/entity")
               (:file "main/view/comp-meta")
               (:file "main/view/view" :depends-on ("main/view/component"
                                                    "main/view/store"
                                                    "main/view/comp-meta"))
               (:file "main/view/lib")
               (:file "main/view/store")
               (:file "main/view/template" :depends-on ("main/view/store"
                                                        "main/view/comp-meta"))
               (:file "main/view/component" :depends-on ("main/view/template"
                                                         "main/view/control-cons"))
               (:file "main/view/control-cons" :depends-on ("main/view/template"))
               (:file "main/view/auto/form")
               (:file "main/auth/form-gen")
               (:file "main/auth/auth" :depends-on ("main/auth/form-gen"
                                                    "main/main"))
               (:file "main/email" :depends-on ("main/config"))
               (:file "main/config")
               (:file "main/main"
                      :depends-on ("main/json-ser"
                                   "main/json-deser"
                                   "main/view/view"
                                   "main/view/lib"
                                   "main/entity"))))

(asdf:defsystem rdf-tests
  :depends-on (:rdf :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "packages")
               (:test-file "test/main")
               (:test-file "test/json")
               (:test-file "test/entity")
               (:test-file "test/view/view")
               (:test-file "test/view/control-cons")
               (:test-file "test/view/component")
               (:test-file "test/auth/auth")
               (:test-file "test/auth/form-gen")
               ))

(asdf:defsystem rdf-full-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :parenscript)
  :components ((:file "packages")
               (:file "examples/full/main")))

(asdf:defsystem rdf-bs-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "packages")
               (:file "examples/bs/main")))

(asdf:defsystem rdf-todo-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "packages")
               (:file "examples/todo/main")))

(asdf:defsystem rdf-auth-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "packages")
               (:file "examples/auth/main")))

(asdf:defsystem rdf-email-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "packages")
               (:file "examples/email/main")))
