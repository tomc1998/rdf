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
   '(:methods
    ((dismiss () (@ (@ (! ".alert") alert) "close")))
)
   '(:bs-container
     (:bs-row
      ((:bs-col :class "my-col" :types (array "sm-12" "lg-6"))
       "Item 1")
      ((:bs-col :class "my-col" :types (array "sm-12" "lg-6"))
       "Item 2")
      )
     (:bs-row
      ((:bs-col :class "my-col" :types (array "sm-12" "md-6" "lg-4"))
       "Item 3")
      ((:bs-col :class "my-col" :types (array "sm-12" "md-6" "lg-4"))
       "Item 4")
      ((:bs-col :class "my-col" :types (array "sm-12" "md-12" "lg-4"))
       "Item 5"))
     ((div class "alert") "Hello" ((button class "btn" onclick {@dismiss}) "Dismiss"))
     )
   )

  ;; Define routes
  (rdf:set-view-routes '(("/" home)))

  ;; Start the server
  (rdf:rdf-stop)
  (rdf:rdf-start)
  )
