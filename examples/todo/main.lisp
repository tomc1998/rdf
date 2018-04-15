(in-package :rdf-todo-example)

(defun model ()
  (rdf:defentity user-auth ((email "VARCHAR(256)" :not-null :unique) (pass "CHAR(116)" :not-null)))
  (rdf:defentity todo ((body "VARCHAR(2048)") (done "TINYINT(1)" :default "0")) :parents (user-auth))
  (rdf:defentity comment ((body "VARCHAR(2048)")) :parents (todo user-auth))
  )

(defun reg-page ()
  (bs:gen-form 'reg-form '(("email" "Email" "Enter your email here")
                           ("pass" "Password" "Enter your password here" :type "password"))
               '(app-req "/reg" (array obj) (lambda () (chain m route (set "/check-email")))))

  (rdf:register-component
   'reg '(:anim-in :fade)
   '((div class "container")
     ((div class "row justify-content-center my-5") ((h1 style "text-align: center")
                                                     (strong "Create your account")))
     ((div class "row justify-content-center mt-5")
      ((div class "col-sm-12 col-lg-8") reg-form)))))

(defun login-page ()
  (bs:gen-form 'login-form '(("email" "Email" "Enter the email you used to create your account")
                             ("pass" "Password" "Enter your password here" :type "password"))
               '(app-req "/login" (array obj)
                 (lambda (id) (setf {!store.session} (create user-id id))
                         (chain m route (set "/")))))
  (rdf:register-component
   'login '(:anim-in :fade)
   '((div class "container")
     ((div class "row justify-content-center my-5") ((h1 style "text-align: center") (strong "Login")))
     ((div class "row justify-content-center mt-5")
      ((div class "col-sm-12 col-lg-8") login-form)))))

(defun check-email-verif-page ()
  (rdf:register-component
   'check-email-verif '(:anim-in :fade)
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
   '(:anim-in :fade
     :lifecycle ((oninit (if {!store.session} (chain m route (set "/"))))))
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
   'nav-bar
   '(:methods
     ((logout () (app-req "/logout" ()
                          (lambda (res) (setf {!store.session} null)
                                  (chain m route (set "/get-started")))))))
   '((div class "row" style (create height "56px"))
     ((nav class "navbar fixed-top navbar-dark bg-dark ")
      ((a class "navbar-brand" href "#!/") "TODO")
      ((button.btn.btn-link.right
        onclick {@logout}
        style (create color "#AAA")) "Logout")
      ))))

(defun todo ()
  (rdf:add-store-action
   'set-done '(todo-id done)
   '(app-req "/set-done" (array (create id todo-id done done))
     (lambda (res)
       (loop for todo in {!store.todos} do
            (if (= (@ todo id) todo-id)
                (progn (setf (@ todo done) done) break)))
       )))
  (rdf:add-store-action
   'delete-todo '(todo-id)
   '(app-req "/delete-todo" (array (create id todo-id))
     (lambda (res)
       (loop for i from 0 to (length {!store.todos}) do
            (if (= (@ {!store.todos} (progn i) id) todo-id)
                (progn (chain {!store.todos} (splice i 1)) break))))))
  (rdf:add-store-action
   'edit-todo '(todo-id body)
   '(app-req "/edit-todo" (array (create id todo-id body body))
     (lambda (res)
       (loop for todo in {!store.todos} do
            (if (= (@ todo id) todo-id)
                (progn (setf (@ todo body) body) break))))))

  (rdf:register-lass
   'todo
   '((.todo-check
      :background-color "#fff"
      :cursor pointer
      :border "1px solid #999"
      :transition background-color 0.1s
      )
     ((:and .todo-check :hover)
      :background-color "#eaeaea")
     (.todo-checked
      :background-color "#bbb"
      :color "#999"
      (.todo-check
       :border "1px solid #ccc"
       :background-color "#bbb")
      )))

  (rdf:register-component
   'todo
   '(:anim-in :fade-l-expand-y :anim-out :fade-l-collapse-y
     :attrs ((todo)) :state ((editing nil) (prev-body-value ""))
     :methods ((on-done () (rdf:dispatch-action set-done (array {todo.id} (not {todo.done}))))
               (on-delete () (rdf:dispatch-action delete-todo {todo.id}))
               (on-edit () (progn (setf {prev-body-value} {todo.body}) (setf {editing} T)))
               (on-view () (progn (setf {!store.showing-todo-modal} {todo})))
               (on-stop-edit () (progn (setf {todo.body} {prev-body-value}) (setf {editing} NIL)))
               (on-edit-submit () (progn (setf {editing} NIL)
                                         (rdf:dispatch-action edit-todo (array {todo.id} {todo.body}))))
               ))
   `((div class (!class "border rounded my-1 my-md-2 my-lg-3"
                        (!if {todo.done} "todo-checked")))
     ;; Todo view
     ((div class "d-flex align-items-center p-3 todo-body"
           style (create overflow-x "auto"))
      ;; Todo body
      (!if
       (not {editing})
       ;; 'checked' box
       (!array ((div class "rounded p-2 todo-check"
                     onclick {@on-done}))
               ((span class "col") {todo.body}))
       ;; Editing controls
       (!array
        ((button class "btn close" onclick {@on-stop-edit}) ,(code-char #x00d7))
        ((form onsubmit {@on-edit-submit} class "m-0 col")
         ((input (!model {todo.body}) type "text" style (create width "100%"))))
        )))
     ;; Todo controls
     (!if (not (or {editing} {todo.done}))
          (div
           ((hr class "my-0"))
           ((div class "d-flex align-items-center justify-content-around todo-controls")
            ((button class "btn btn-link btn-sm" onclick {@on-view}) "View details")
            ((button class "btn btn-link btn-sm" onclick {@on-delete}) "Delete")
            ((button class "btn btn-link btn-sm" onclick {@on-edit}) "Edit")
            )))
     )
   ))

(defun todo-comment ()
  (rdf:register-component
   'todo-comment '(:anim-in :fade-t-expand-y :anim-out :fade-t-collapse-y :attrs ((comment)))
   '((.media.border.rounded.my-1.p-2 style (create width "100%"))
     (.media-body {comment.body}))
   )
  )

(defun view-todo-modal ()
  (rdf:add-store-action
   'fetch-comments '(todo-id)
   '(app-req "/get-comments" todo-id
     (lambda (res) (loop for c in res do (setf (@ {!store.comments} (@ c id)) c)))))
  (rdf:add-store-action
   'add-comment '(todo-id comment)
   '(app-req "/add-comment" (create body (@ comment body) parent-todo-id todo-id)
     (lambda (res) (rdf:dispatch-action fetch-comments (array todo-id)))))
  (rdf:register-component
   'view-todo-modal
   '(:state ((comment (create body "")) (already-showing nil))
     :methods ((add-comment ()
                (progn
                  (rdf:dispatch-action
                   add-comment (array {!store.showing-todo-modal.id} {comment}))
                  (setf {comment.body} "")))
               (on-close () (setf {!store.showing-todo-modal} null)))
     :lifecycle
     ((onupdate
       (if {!store.showing-todo-modal}
           (progn
             (chain ($ "#todo-modal") (modal "show"))
             (if (not {already-showing})
                 (rdf:dispatch-action fetch-comments (array {!store.showing-todo-modal.id})))
             (setf {already-showing} t))
           (progn
             (chain ($ "#todo-modal") (modal "hide"))
             (setf {already-showing} nil))))))
   `((div class "modal fade" role "dialog" id "todo-modal")
     ((div class "modal-dialog" role "document")
      (!if {!store.showing-todo-modal}
           ((div class "modal-content")
            ((div class "modal-header")
             ((div class "modal-title") {!store.showing-todo-modal.body})
             ((button type "button" class "close" onclick {@on-close})
              ,(code-char #x00d7))
             )
            ((div class "modal-body")
             (.container
              (.row
               ((form.col onsubmit {@add-comment})
                ((input type "text" style (create width "100%")
                        (!model {comment.body}) placeholder "Add a comment..."))))
              (!if (= 0 (length {!store.showing-todo-modal-comments}))
                   (.row.justify-content-center
                    (.p-2 "No comments here yet."))
                   (.row.justify-content-stretch
                    (!loop for comment in {!store.showing-todo-modal-comments}
                           ((todo-comment comment {comment})))))
              )
             )))))))

(defun home-page ()
  (rdf:add-store-action
   'fetch-todos ()
   '(app-req "/get-todos" () (lambda (res) (setf {!store.todos} res))))
  (rdf:register-component
   'add-todo-form
   '(:anim-in :fade-l
     :state ((todo (create body "" done false)))
     :attrs ((onsubmit))
     :methods ((submit () (progn ({onsubmit} {todo}) (setf {todo.body} "")))))
   '((form onsubmit {@submit}) ((input style (create width "100%")
                                 (!model {todo.body})
                                 type "text"
                                 placeholder "Add an item"))))
  (rdf:register-component
   'home
   '(
     :lifecycle ((oninit (progn
                           (if (not {!store.session})
                               (chain m route (set "/get-started"))
                               (rdf:dispatch-action fetch-todos)))))
     :methods ((delete-all-done ()
                (app-req "/delete-all-todos" (array {!store.todos-done})
                         (lambda (res) (rdf:dispatch-action fetch-todos))))
               (add-todo (todo)
                (app-req "/add-todo" (array todo)
                         (lambda (res) (rdf:dispatch-action fetch-todos))))))
   '(div
     ((div class "container-fluid px-0")
      nav-bar
      view-todo-modal
      ((div class "row mt-3 justify-content-center")
       ((div class "col-lg-6 col-md-8 col-sm-12")
        ((add-todo-form onsubmit {@add-todo}))))
      ((div class "row justify-content-center")
       ((div class "col-sm-12 col-md-8 col-lg-6")
        (!loop for todo in {!store.todos-not-done}
               ((todo key {todo.id} todo {todo})))))
      (!if (> (length {!store.todos-done}) 0)
       (div
        (hr)
        ((div class "row justify-content-center")
         ((button class "btn btn-white" onclick {@delete-all-done}) "Delete all completed tasks"))
        ((div class "row justify-content-center")
         ((div class "col-sm-12 col-md-8 col-lg-6")
          (!loop for todo in {!store.todos-done}
                 ((todo key {todo.id} todo {todo})))))))
      ))))

(defun client ()
  (rdf:clear-additional-scripts)
  (rdf:clear-additional-stylesheets)
  ;; Load Bootstrap
  (bs:load-all)
  ;; Setup all components & routes
  (rdf:add-initial-store-state 'session 'null)
  (rdf:add-initial-store-state 'todos '(array))
  (rdf:add-initial-store-state 'comments '(create))
  (rdf:add-initial-store-state 'showing-todo-modal 'null)
  (rdf:add-store-computed 'showing-todo-modal-comments
                          ;; Loop through all the comments, find the ones which
                          ;; match the current todo
                          '(let ((comments (array)))
                            (for-in (comment-id {!store.comments})
                             (let ((comment (@ {!store.comments} (progn comment-id))))
                               (if (= (@ comment parent-todo-id) {!store.showing-todo-modal.id})
                                   (chain comments (push comment)))))
                            comments))
  (rdf:add-store-computed 'todos-done '(loop for todo in {!store.todos}
                                          if (@ todo done) collect todo))
  (rdf:add-store-computed 'todos-not-done '(loop for todo in {!store.todos}
                                              if (not (@ todo done)) collect todo))
  (nav-bar)
  (todo)
  (todo-comment)
  (view-todo-modal)
  (home-page)
  (splash-page)
  (reg-page)
  (login-page)
  (check-email-verif-page)
  (rdf:set-view-routes '(("/get-started" splash)
                         ("/" home)
                         ("/reg" reg)
                         ("/check-email" check-email-verif)
                         ("/login" login)
                         ))
  ;; Set error behaviour
  (rdf:set-client-default-unauthorized-behaviour '(progn (setf {!store.session} null)
                                                   (chain m route (set "/login")))))

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
  (rdf:define-app-req "/logout" ()
    (lambda () (setf (rdf:session-value 'user-id) nil)) :require-auth t)

  (rdf:define-app-req "/add-todo" (todo)
    (lambda (todo)
      (setf (slot-value todo 'parent-user-auth-id) (rdf:session-value 'user-id))
      (rdf:insert-one todo)) :require-auth t)
  (rdf:define-app-req "/get-todos" ()
    (lambda ()
      ;; Select all todos belonging to this user
      (let ((tree (rdf:select-tree
                   '(todo) :where `(= ,(rdf:session-value 'user-id)
                                      (todo parent-user-auth-id)))))
        (mapcar #'car tree)))
    :require-auth t)
  (rdf:define-app-req "/set-done" (todo)
    (lambda (todo) (if (rdf:check-owner-eq todo 'parent-user-auth-id (rdf:session-value 'user-id))
                       (rdf:update-entity todo 'done)) ()) :require-auth t)
  (rdf:define-app-req "/delete-todo" (todo)
    (lambda (todo) (if (rdf:check-owner-eq todo 'parent-user-auth-id (rdf:session-value 'user-id))
                       (rdf:delete-entity todo)) ()) :require-auth t)
  (rdf:define-app-req "/delete-all-todos" ((todo))
    (lambda (todos) (loop for todo in todos do
                         (if (rdf:check-owner-eq todo 'parent-user-auth-id (rdf:session-value 'user-id))
                             (rdf:delete-entity todo))) ()) :require-auth t)
  (rdf:define-app-req "/edit-todo" (todo)
    (lambda (todo) (if (rdf:check-owner-eq todo 'parent-user-auth-id (rdf:session-value 'user-id))
                       (rdf:update-entity todo 'body)) ()) :require-auth t)
  (rdf:define-app-req "/add-comment" (comment)
    (lambda (comment) (if (rdf:check-owner-eq
                           (make-instance 'todo :id (slot-value comment 'parent-todo-id))
                           'parent-user-auth-id (rdf:session-value 'user-id))
                          ;; Set the author ID
                          (progn (setf (slot-value comment 'parent-user-auth-id)
                                       (rdf:session-value 'user-id))
                                 ;; Insert the comment
                                 (rdf:insert-one comment))) ()) :require-auth t)
  (rdf:define-app-req "/get-comments" (nil)
    (lambda (todo-id)
      (let ((tree (rdf:select-tree '(comment) :where
                                   `(= ,todo-id (comment parent-todo-id)))))
        (mapcar #'car tree))) :require-auth t)
  )

(defun main ()
  (model)
  (client)
  (server)
  (rdf:rdf-stop)
  (rdf:rdf-start 80))
