
(in-package :rdf-todo-example)

(defun main ()
  (rdf:clear-additional-scripts)
  (rdf:clear-additional-stylesheets)

  (bs:load-all)

  (rdf:register-component
   'splash
   ()
   '(div
     ((div class "jumbotron")
      (h1 "Todo app")
      ((a class "btn btn-primary" href "#!/reg") "Get started")
      )))

  (rdf:set-view-routes '(("/" splash)))

  (rdf:rdf-stop)
  (rdf:rdf-start)
  )
