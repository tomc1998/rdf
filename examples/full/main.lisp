(in-package :rdf-full-example)

(defun define-entities ()
(rdf:defentity user-auth ((user "VARCHAR(256)" :not-null) (pass "VARCHAR(256)" :not-null)) () t))

(defun register-style ())

(defun register-components ()
  ;; Set default client response for unauth requests
  (rdf:set-client-default-unauthorized-behaviour '(progn (alert "UNAUTH") (alert "Hello")))

  ;; Create components

  (rdf:register-component
   :test-children ()
   '(div {children}))

  (rdf:register-component
   :test-if '(:attrs (apply-class))
   '((div class ($if {apply-class} "my-class"))
     ($if {apply-class}
      (span "Applying class:" {apply-class})
      (span "Not applying class"))))

  (rdf:register-component
   :reg-form
   '(:state (user (create user "" pass ""))
     :methods ((reg (e) (progn (chain e (prevent-default))
                               (app-req "/reg" {user} (lambda (res code)))))))
   '((form onsubmit {@reg})
     (label "Username")
     ((input type "text" ($model {user.user}))) (br)
     (label "Password")
     ((input type "password" ($model {user.pass}))) (br)
     ((input type "submit"))))

  (rdf:register-component
   :login-form
   '(:state (user (create user "" pass ""))
     :methods ((login (e) (progn (chain e (prevent-default))
                                 (app-req "/login" {user} (lambda (res code)
                                                            (chain console (log res))))))))
   '((form onsubmit {@login})
     (label "Username")
     ((input type "text" ($model {user.user}))) (br)
     (label "Password")
     ((input type "password" ($model {user.pass}))) (br)
     ((input type "submit"))))

  (rdf:register-component
   :home
   '(:methods ((access-data () (app-req "/access-data" (array) (lambda (res code) (alert res))))))
   '(div
     (h1 "Register")
     :reg-form
     (h1 "Login")
     :login-form
     ((button onclick {@access-data}) "Access sensitive data")
     (:test-children ((div class ($if 1 "asdtrue" "asdfalse")) "hello"))
     ((:test-if apply-class t))
     ((:test-if apply-class nil)))))

(defun setup-routes ()
  (rdf:set-view-routes '(("/" home))))

(defun setup-app-req ()
  ;; Set verify auth function to check for user-id
  (setf rdf:*verify-auth* (lambda (type)
                            (cond ((eq type 'normal) (rdf:session-value 'user-id)))))
  (rdf:define-app-req "/reg" (user-auth)
    (lambda (user)
      ;; Hash pwd & insert, returning the user's ID
      (setf (slot-value user 'pass)
            (rdf:hash-pwd (rdf:string-to-octets (slot-value user 'pass))))
      (rdf:insert-one user)))
  (rdf:define-app-req "/login" (user-auth)
    (lambda (user)
      ;; Select user with the right name
      (let* ((tree (rdf:select-tree '(user-auth ())
                                    :where `(= (user-auth user) ,(slot-value user 'user))))
             (first (car tree)))
        ;; Make sure user exists - if not, just return nil
        (if (not first) nil
            (let ((db-user (car first)))
              ;; Check pwd hash, return true & set session if it worked
              (if (rdf:check-pwd (rdf:string-to-octets (slot-value user 'pass))
                                 (slot-value db-user 'pass))
                  (progn (setf (rdf:session-value 'user-id) (slot-value db-user 'rdf:id))
                         T)
                  NIL))))))
  (rdf:define-app-req "/access-data" ()
    (lambda () "This is some sensitive data") :require-auth 'normal))

(defun main ()
  (define-entities)
  (register-components)
  (register-style)
  (setup-routes)
  (setup-app-req)
  (rdf:rdf-stop)
  (rdf:rdf-start))
