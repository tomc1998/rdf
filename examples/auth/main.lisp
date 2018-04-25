(in-package :rdf-auth-example)

(defun main ()
  (bs:load-all)

  (rdf:setup-auth '((first-name "VARCHAR(256)") (last-name "VARCHAR(256)"))
                  :auth-types '(:email-password) :override t)

  (rdf:gen-form
   'login-form
   '(((first-name "First name" :maxLength 32 :required t)
      (last-name "Last name" :maxLength 32 :required t))
     ((email "Email" :required t
       :placeholder "Enter your email here" :type "email"))
     ((password "Password (min. 8 characters)" :pattern ".{8,}" :required t
       :placeholder "Enter your password here" :type "password")))
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
