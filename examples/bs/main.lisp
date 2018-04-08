(in-package :rdf-bs-example)

(defun main ()
  ;; Make sure we don't have any additional stylesheets/scripts loaded already
  (rdf:clear-additional-stylesheets)
  (rdf:clear-additional-scripts)
  ;; Load bootstrap components
  (bs:load-all)

  ;; Define our own component
  (rdf:register-component 'home () '(:bs-container (h1 "Hello")))

  ;; Define routes
  (rdf:set-view-routes '(("/" home)))

  ;; Start the server
  (rdf:rdf-stop)
  (rdf:rdf-start)
  )
