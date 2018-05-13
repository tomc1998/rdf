(defpackage :rdf
  (:use  "PARENSCRIPT" "COMMON-LISP")
  (:shadowing-import-from
   :corm get-conn entity-already-exists select-tree insert-one id
   insert-duplicate-error update-all update-entity delete-entity delete-all check-owner-eq
   def-many-to-many connect disconnect
   )
  (:shadowing-import-from :hunchentoot :session-value :log-message*)
  (:export rdf-start
           rdf-stop
           *server-ref*
           *verify-auth*

           ;; Config shit
           *base-url*

           ;; Auth system
           user-id
           user-info
           user-auth
           setup-auth
           auto-login-form
           auto-reg-form
           *auto-login-form-callback*
           *auto-reg-form-callback*

           ;; Emails
           send-email
           send-email-to-user
           *smtp-server*
           *smtp-port*
           *smtp-auth*
           *email-domain*

           ;; Request stuff
           define-app-req
           define-file-handler
           app-req-error
           raise-app-error

           ;; View (i.e. client) stuff
           defcomp
           register-component
           set-view-routes
           add-initial-store-state
           add-store-action
           add-store-computed
           dispatch-action
           entity-to-json
           entity-from-json
           set-client-default-unauthorized-behaviour
           *error-component*
           ;; View styling
           register-lass 
           clear-lass
           ;; External view stuff
           add-additional-stylesheet-url
           clear-additional-stylesheets
           add-additional-script-url
           clear-additional-scripts

           ;; Auto-gen view stuff
           gen-form
           *gen-form-func*

           ;; Corm re-exports
           defentity ; Not actually a re-export - see entity.lisp
           entity-already-exists
           select-tree
           insert-one
           insert-duplicate-error
           update-entity
           update-all
           delete-entity
           delete-all
           check-owner-eq
           def-many-to-many
           connect
           disconnect
           id
           get-conn

           ;; Hunchentoot session re-exports
           session-value
           log-message*

           ;; Re-export ironclad hash password & flexi stream functions
           hash-pwd
           check-pwd
           ))

(defpackage :rdf-full-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(defpackage :rdf-auth-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(defpackage :rdf-bs-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(defpackage :rdf-todo-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(defpackage :rdf-email-example
  (:use "PARENSCRIPT" "COMMON-LISP")
  (:export :main))

(asdf:defsystem rdf
  :description "A rapid development web framework"
  :depends-on (:corm :hunchentoot :cl-json :parenscript :str :ironclad :flexi-streams :lass :cl-smtp)
  :components ((:file "main/json-ser")
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
               (:file "main/auth/auth" :depends-on ("main/auth/form-gen"))
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
  :components ((:test-file "test/main")
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
  :components ((:file "examples/full/main")))

(asdf:defsystem rdf-bs-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "examples/bs/main")))

(asdf:defsystem rdf-todo-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "examples/todo/main")))

(asdf:defsystem rdf-auth-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "examples/auth/main")))

(asdf:defsystem rdf-email-example
  :author "Tom <thomascheng1998@gmail.com>"
  :depends-on (:rdf :bootstrap :parenscript)
  :components ((:file "examples/email/main")))
