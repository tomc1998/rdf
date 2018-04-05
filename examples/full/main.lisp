(in-package :rdf-full-example)

(defun define-entities ()
(rdf:defentity user-auth ((user "VARCHAR(256)" :not-null) (pass "VARCHAR(256)" :not-null)) () t))

(defun register-style ()
  (let ((primary "#F00"))
    (rdf:register-lass
     'app-style
     `(div :background-color ,primary)))
  )

(defun register-components ()
  (rdf:register-component
   :nav '(:methods
          ((reg ()
            (app-req "/reg" (array (create user "Tom" pass "pwd")) (lambda (res code) (alert code))))
           (list-users ()
            (app-req "/get-users" (array)
                     (lambda (res code)
                       (chain console (log res))
                       (let ((users (loop for u in (@ res users)
                                       collect (@ u user))))
                         (alert users))
                       )))))
   '(div
     ((a href "/#!") "Home")
     ((a href "/#!/about") "About")
     ((button onclick {@reg}) "Register")
     ((button onclick {@list-users}) "List users")
     ))

  (rdf:add-initial-store-state 'home '(create count 0))
  (rdf:add-initial-store-state 'about '(create count 0))

  (rdf:register-component
   :home
   '(:state (big-object (create a 1 b 2 c 3))
     :methods ((inc () (setf {$store.home.count} (1+ {$store.home.count})))))
   '(div :nav (div "Hello, this is the home page.")
     ((button onclick {@inc}) "You've pressed this button " {$store.home.count} " times.")
     (br)
     "A: " {big-object.a} (br)
     "B: " {big-object.b} (br)
     "C: " {big-object.c} (br)
     ))

  (rdf:register-component
   :about
   '(:methods ((inc () (setf {$store.about.count} (1+ {$store.about.count})))))
   '(div :nav (div "Hello, this is the about page. It has a much bigger button.")
     ((button style "font-size: 30px" onclick {@inc})
      "You've pressed this button " {$store.about.count} " times."))))

(defun setup-routes ()
  (rdf:set-view-routes '(("/" home)
                         ("/about" about))))

(defun setup-app-req ()
  (rdf:define-app-req "/reg" (user-auth)
    (lambda (user)
      ;; Hash pwd & insert, returning the user's ID
      (setf (slot-value user 'pass)
            (rdf:hash-pwd (rdf:string-to-octets (slot-value user 'pass))))
      (rdf:insert-one user)))
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
