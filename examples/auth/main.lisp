(in-package :rdf-auth-example)

(defun main ()
  (rdf:setup-auth '((first-name "VARCHAR(256)") (last-name "VARCHAR(256)"))
                  :auth-types '(:email-password) :override t)

  (rdf:register-component
   'home '(:methods
           ((register () (app-req "/rdf/register" (array
                                                   (create email "asd" password "asd")
                                                   (create first-name "asd" last-name "asd"))))
            (login () (app-req "/rdf/login" (create email "asd" password "asd")))))
   '(div
     ((button onclick {@register}) "Try register")
     ((button onclick {@login}) "Try login")
     )
   )

  (rdf:set-view-routes '(("/" home)))

  (rdf:rdf-stop)
  (rdf:rdf-start 4242)
  )
