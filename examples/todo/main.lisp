
(in-package :rdf-todo-example)

(defun model ()
  (rdf:defentity user-auth ((email "VARCHAR(256)" :not-null :unique) (pass "CHAR(116)" :not-null)) () T))

(defun reg-page ()
  (bs:gen-form 'reg-form '(("email" "Email" "Enter your email here")
                           ("pass" "Password" "Enter your password here" :type "password"))
               '(app-req "/reg" (array obj) (lambda () (alert "Registered"))))

  (rdf:register-component
   'reg ()
   '((div class "container")
     ((div class "row justify-content-center my-5") (h1 (strong "TODO")))
     ((div class "row justify-content-center mt-5")
      ((div class "col-sm-12 col-lg-8") :reg-form)))))

(defun splash-page ()
  (rdf:register-component
   'splash
   '(:lifecycle ((onbeforeupdate (if {!store.session} (chain m route (set "/home"))))))
   '(div
     ((div class "jumbotron")
      ((h1 class "display-4") "TODO")
      ((p class "lead") "A todo-list application created with the cl-rdf framework")
      (hr)
      ((p class "lead")
       ((a class "btn btn-primary btn-lg" role "button" href "#!/reg") "Get started"))
      ))))

(defun home-page ()
  (rdf:register-component
   'home ()
   '((div class "container") ((nav class "navbar fixed-top navbar-dark bg-dark ")
                              ((a class "navbar-brand" href "#!/") "Hello")))))

(defun client ()
  (rdf:clear-additional-scripts)
  (rdf:clear-additional-stylesheets)
  ;; Load Bootstrap
  (bs:load-all)
  ;; Setup all components & routes
  (rdf:add-initial-store-state 'session nil)
  (home-page)
  (splash-page)
  (reg-page)
  (rdf:set-view-routes '(("/" splash)
                         ("/home" home)
                         ("/reg" reg))))

(defun server ()
  (rdf:define-app-req "/reg" (user-auth)
    (lambda (user-auth)
      (setf (slot-value user-auth 'pass) (rdf:hash-pwd (slot-value user-auth 'pass)))
      (rdf:log-message* :INFO "~a" (length (slot-value user-auth 'pass)))
      (handler-case (rdf:insert-one user-auth)
        (rdf:insert-duplicate-error () (rdf:raise-app-error "Email taken" 400))))))

(defun main ()
  (model)
  (client)
  (server)
  (rdf:rdf-stop)
  (rdf:rdf-start)
  )
