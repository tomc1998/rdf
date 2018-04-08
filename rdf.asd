(ql:quickload :prove)
(ql:quickload :str)
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)
(ql:quickload :parenscript)
(ql:quickload :ironclad)
(ql:quickload :flexi-streams)
(ql:quickload :lass)
(ql:quickload :corm)

(defpackage :rdf
  (:use  "PARENSCRIPT" "COMMON-LISP")
  (:shadowing-import-from :corm :entity-already-exists :select-tree :insert-one :id)
  (:shadowing-import-from :hunchentoot :session-value :log-message*)
  (:export :rdf-start
           :rdf-stop
           :*server-ref*
           :*verify-auth*
           :add-additional-stylesheet-url
           :clear-additional-stylesheets
           :add-additional-script-url
           :clear-additional-scripts

           ;; Request stuff
           :define-app-req
           :app-req-error
           :raise-app-error

           ;; View (i.e. client) stuff
           :register-component
           :set-view-routes
           :add-initial-store-state
           :entity-to-json
           :entity-from-json
           :set-client-default-unauthorized-behaviour
           :*error-component*
           ;; View styling
           :register-lass 
           :clear-lass

           ;; Corm re-exports
           :defentity ; Not actually a re-export - see entity.lisp
           :entity-already-exists
           :select-tree
           :insert-one
           :id

           ;; Hunchentoot session re-exports
           :session-value
           :log-message*

           ;; Re-export ironclad hash password & flexi stream functions
           :hash-pwd
           :check-pwd
           :string-to-octets
           ))

(defpackage :rdf-full-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(defpackage :rdf-bs-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(asdf:defsystem rdf
  :description "A rapid development web framework"
  :depends-on (:corm :hunchentoot :cl-json :parenscript :str :ironclad :flexi-streams :lass)
  :components ((:file "main/json-ser")
               (:file "main/json-deser")
               (:file "main/entity")
               (:file "main/view/view" :depends-on ("main/view/control-cons"))
               (:file "main/view/lib")
               (:file "main/view/control-cons")
               (:file "main/main"
                      :depends-on ("main/json-ser" "main/json-deser"
                                                   "main/view/view" "main/view/lib" "main/entity")
                      )))

(asdf:defsystem rdf-tests
  :depends-on (:rdf :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main")
               (:test-file "test/json")
               (:test-file "test/entity")
               (:test-file "test/view/view")))

(asdf:defsystem rdf-full-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf)
  :components ((:file "examples/full/main")))

(asdf:defsystem rdf-bs-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap)
  :components ((:file "examples/bs/main")))
