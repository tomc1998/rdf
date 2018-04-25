(in-package :rdf-auth-example)

(defun main ()
  (rdf:clear-additional-stylesheets)
  (rdf:clear-additional-scripts)
  (bs:load-all)

  (rdf:setup-auth '((first-name "VARCHAR(256)") (last-name "VARCHAR(256)"))
                  :auth-types '(:email-password) :override t)

  (rdf:register-component
   'home '(:methods
           ((register () (app-req "/rdf/register" (array
                                                   (create email "asd" password "asd")
                                                   (create first-name "asd" last-name "asd"))))
            (login () (app-req "/rdf/login" (create email "asd" password "asd")))))
   '(.container
     (h1 "Register")
     (.row.justify-content-center (.col-sm-12.col-md-8.col-lg-6 rdf:auto-reg-form))
     (h1 "Login")
     (.row.justify-content-center (.col-sm-12.col-md-8.col-lg-6 rdf:auto-login-form))
     ))

  (rdf:set-view-routes '(("/" home)))

  (rdf:rdf-stop)
  (rdf:rdf-start 4242)
  )
