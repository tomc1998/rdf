(in-package :rdf-full-example)

(defun define-entities ()
(rdf:defentity user-auth ((user "VARCHAR(256)" :not-null) (pass "VARCHAR(256)" :not-null)) () t))

(defun register-style ()
  
  )

(defun register-components ()
  (rdf:register-component
   :reg-form
   '(:state (user (create user "" pass ""))
     :methods ((reg (e) (progn (chain e (prevent-default))
                               (app-req "/reg" {user} (lambda (res code)))))
               (set-user (val) (setf {user.user} val))
               (set-pass (val) (setf {user.pass} val))))
   '((form onsubmit {@reg})
     (label "Username")
     ((input type "text"
       onchange ($ (chain m (with-attr "value" {@set-user})))
       value {user.user})) (br)
     (label "Password")
     ((input type "password"
       onchange ($ (chain m (with-attr "value" {@set-pass})))
       value {user.pass})) (br)
     ((input type "submit"))))

  (rdf:register-component
   :login-form
   '(:state (user (create user "" pass ""))
     :methods ((login (e) (progn (chain e (prevent-default))
                               (app-req "/login" {user} (lambda (res code)))))
               (set-user (val) (setf {user.user} val))
               (set-pass (val) (setf {user.pass} val))))
   '((form onsubmit {@login})
     (label "Username")
     ((input type "text"
       onchange ($ (chain m (with-attr "value" {@set-user})))
       value {user.user})) (br)
     (label "Password")
     ((input type "password"
       onchange ($ (chain m (with-attr "value" {@set-pass})))
       value {user.pass})) (br)
     ((input type "submit"))))

  (rdf:register-component
   :home
   '(:methods ((reg () )))
   '(div
     (h1 "Register")
     :reg-form
     (h1 "Login")
     :login-form
     )))

(defun setup-routes ()
  (rdf:set-view-routes '(("/" home))))

(defun setup-app-req ()
  (rdf:define-app-req "/reg" (user-auth)
    (lambda (user)
      ;; Hash pwd & insert, returning the user's ID
      (setf (slot-value user 'pass)
            (rdf:hash-pwd (rdf:string-to-octets (slot-value user 'pass))))
      (rdf:insert-one user)))
  (rdf:define-app-req "/login" (user-auth)
    (lambda (user)))

  (rdf:define-app-req "/get-users" ()
    (lambda () (list :users (loop for u in (rdf:select-tree '(user-auth ()))
                               collect (rdf:entity-to-json (car u)))))))

(defun main ()
  (define-entities)
  (register-components)
  (register-style)
  (setup-routes)
  (setup-app-req)
  (rdf:rdf-stop)
  (rdf:rdf-start))
