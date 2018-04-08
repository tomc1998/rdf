
(in-package :rdf-todo-example)

(defun main ()
  (rdf:clear-additional-scripts)
  (rdf:clear-additional-stylesheets)

  (bs:load-all)

  (rdf:add-initial-store-state 'session nil)

  (rdf:register-component
   'splash
   '(:lifecycle ((onbeforeupdate (if {!store.session} (chain m route (set "/home"))))))
   '(div
     ((div class "jumbotron")
      (h1 "Todo app")
      ((a class "btn btn-primary" href "#!/reg") "Get started")
      )))

  (rdf:register-component
   'home ()
   '(div "Hello"))

  (rdf:set-view-routes '(("/" splash) ("/home" home)))

  (rdf:rdf-stop)
  (rdf:rdf-start)
  )
