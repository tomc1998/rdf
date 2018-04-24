(in-package :rdf-auth-example)

(defun main ()
  (rdf:setup-auth '((first-name "VARCHAR(256)") (last-name "VARCHAR(256)"))
                  :auth-types '(:email-password) :override t)

  (rdf:gen-form
   'login-form
   '(((first-name "First name")
      (last-name "Last name"))
     ((email "Email" :placeholder "Enter your email here" :type "email"))
     ((password "Password" :placeholder "Enter your password here" :type "password")))
   '(chain console (log obj)))

  (rdf:register-component
   'home '(:methods
           ((register () (app-req "/rdf/register" (array
                                                   (create email "asd" password "asd")
                                                   (create first-name "asd" last-name "asd"))))
            (login () (app-req "/rdf/login" (create email "asd" password "asd")))))
   '(div
     login-form
     ((button onclick {@register}) "Try register")
     ((button onclick {@login}) "Try login")
     )
   )

  (rdf:set-view-routes '(("/" home)))

  (rdf:rdf-stop)
  (rdf:rdf-start 4242)
  )
