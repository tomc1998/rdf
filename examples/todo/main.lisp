
(in-package :rdf-todo-example)

(defun model ()
  (rdf:defentity user-auth ((email "VARCHAR(256)" :not-null :unique) (pass "CHAR(116)" :not-null)) () T)
  (rdf:defentity todo ((body "VARCHAR(2048)") (done "TINYINT(1)" :default "0")) (user-auth) T))

(defun reg-page ()
  (bs:gen-form 'reg-form '(("email" "Email" "Enter your email here")
                           ("pass" "Password" "Enter your password here" :type "password"))
               '(app-req "/reg" (array obj) (lambda () (chain m route (set "/check-email")))))

  (rdf:register-component
   'reg ()
   '((div class "container")
     ((div class "row justify-content-center my-5") ((h1 style "text-align: center") (strong "Create your account")))
     ((div class "row justify-content-center mt-5")
      ((div class "col-sm-12 col-lg-8") :reg-form)))))

(defun login-page ()
  (bs:gen-form 'login-form '(("email" "Email" "Enter the email you used to sign up")
                             ("pass" "Password" "Enter your password"))
               '(app-req "/login" (array obj)
                 (lambda (id) (setf {!store.session.user-id} id)
                         (chain m route (set "/home")))))
  (rdf:register-component
   'login ()
   '((div class "container")
     ((div class "row justify-content-center my-5") ((h1 style "text-align: center") (strong "Login")))
     ((div class "row justify-content-center mt-5")
      ((div class "col-sm-12 col-lg-8") :login-form)))))

(defun check-email-verif-page ()
  (rdf:register-component
   'check-email-verif ()
   '((div class "container")
     ((div class "row justify-content-center my-5") (h1 (strong "TODO")))
     ((div class "row mt-5 mb-3 justify-content-center")
      ((div class "my-auto") "Check your email for the verification code"))
     ((div class "row justify-content-center")
      ((a href "#!/login" class "btn btn-link") "I already verified my email!"))
     )))

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
       (div ((a class "btn btn-primary btn-lg" role "button" href "#!/reg") "Get started"))
       (div ((a class "btn btn-link px-0" role "button" href "#!/login") "I already have an account")))
      ))))

(defun nav-bar ()
  (rdf:register-component
   'nav-bar ()
   '((div class "row" style (create height "56px"))
     ((nav class "navbar fixed-top navbar-dark bg-dark ")
                        ((a class "navbar-brand" href "#!/") "TODO")))))

(defun home-page ()
  (rdf:register-component
   'add-todo-form
   '(:state (todo (create))
     :attrs (onsubmit)
    :methods ((submit () ({onsubmit} {todo}))))
   '((form onsubmit {@onsubmit}) ((input (!model {todo.body}) type "text" placeholder "Add a todo"))))
  (rdf:register-component
   'home
   '(:methods ((add-todo (todo) (app-req "/add-todo" (array todo) (lambda (res) (alert res)))))
          )
   '((div class "container")
     :nav-bar
     ((div class "row") (h2 "Your TODO list"))
     ((div class "row") ((:add-todo-form onsubmit {@add-todo})))
     ((div class "row")
      ((ul class "list-group")
       (!loop for todo in {!store.todos}
              ((li class "list-group-item") {todo}))))
     )
   ))

(defun client ()
  (rdf:clear-additional-scripts)
  (rdf:clear-additional-stylesheets)
  ;; Load Bootstrap
  (bs:load-all)
  ;; Setup all components & routes
  (rdf:add-initial-store-state 'session '(create))
  (rdf:add-initial-store-state 'todos '(array))
  (nav-bar)
  (home-page)
  (splash-page)
  (reg-page)
  (login-page)
  (check-email-verif-page)
  (rdf:set-view-routes '(("/" splash)
                         ("/home" home)
                         ("/reg" reg)
                         ("/check-email" check-email-verif)
                         ("/login" login)
                         ))
  ;; Set error behaviour
  (rdf:set-client-default-unauthorized-behaviour '(chain m route (set "/login"))))

(defun server ()
  (setf rdf:*verify-auth* (lambda (type) (declare (ignore type)) (rdf:session-value 'user-id)))
  (rdf:define-app-req "/reg" (user-auth)
    (lambda (user-auth)
      (setf (slot-value user-auth 'pass) (rdf:hash-pwd (slot-value user-auth 'pass)))
      (handler-case (rdf:insert-one user-auth)
        (rdf:insert-duplicate-error () (rdf:raise-app-error "Email taken" 400)))))
  (rdf:define-app-req "/login" (user-auth)
    (lambda (user-auth)
      (let ((users (rdf:select-tree '(user-auth ()) :where
                                    `(= (user-auth email) ,(slot-value user-auth 'email)))))
        (if (not users) (rdf:raise-app-error "Incorrect email or password" 400))
        (let ((pwd-hash (slot-value (caar users) 'pass)))
          (if (not (rdf:check-pwd (slot-value user-auth 'pass) pwd-hash))
              (rdf:raise-app-error "Incorrect email or password" 400)))
        (setf (rdf:session-value 'user-id) (slot-value (caar users) 'rdf:id))
        (slot-value (caar users) 'rdf:id))))
  (rdf:define-app-req "/add-todo" (todo)
    (lambda (todo)
      (setf (slot-value todo 'parent-user-auth-id) (rdf:session-value 'user-id))
      (rdf:insert-one todo)) :require-auth t)
  )

(defun main ()
  (model)
  (client)
  (server)
  (rdf:rdf-stop)
  (rdf:rdf-start))
