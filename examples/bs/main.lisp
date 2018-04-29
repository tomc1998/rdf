(in-package :rdf-bs-example)

(defun main ()
  ;; Make sure we don't have any additional stylesheets/scripts loaded already
  (rdf:clear-additional-stylesheets)
  (rdf:clear-additional-scripts)
  ;; Load bootstrap components
  (bs:load-all)

  ;; Load our own styles
  (rdf:clear-lass 'my-lass)
  (rdf:register-lass 'my-lass '((.my-col
                                 :border "1px solid #444"
                                 :background-color "#888"
                                 :padding "4px")))

  ;; Define our own component
  (rdf:register-component
   'home
   ()
   `((div class "container")
     ((div class "row")
      ((div class "my-col col-sm-12 col-lg-6 col-lg-6")
       "Item 1")
      ((div class "my-col col-sm-12 col-lg-6 col-lg-6")
       "Item 2")
      )
     ((div class "row")
      ((div class "my-col col-sm-12 col-md-6 col-lg-4")
       "Item 3")
      ((div class "my-col col-sm-12 col-md-6 col-lg-4")
       "Item 4")
      ((div class "my-col col-sm-12 col-md-12 col-lg-4")
       "Item 5"))
     ((div class "alert alert-warning alert-dismissible fade show" role "alert")
      "Hello"
      ((button class "close" "data-dismiss" "alert") ,(code-char #x00d7)))
     )
   )

  ;; Define routes
  (rdf:set-view-routes '(("/" home)))

  ;; Start the server
  (rdf:rdf-stop)
  (rdf:rdf-start :port 4242)
  )
